# fmp_geometry.R
# Parsers for Flood Modeller Pro spatial files.
# Returns sf objects in EPSG:27700 (BNG) throughout.
#
# Functions exported:
#   parse_gxy()       -- parse a .gxy file -> LINESTRING sf per reach
#   parse_dat()       -- parse a .dat file -> data.table of hydraulic units with list-column data
#                        (use as_lines=TRUE for spatial LINESTRING fallback in model_geometry)
#   model_geometry()  -- wrapper: tries GXY, falls back to DAT, attaches model_id

library(sf)
library(data.table)
library(cli)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.bng <- 27700L

# Strip inline comments and blank lines from a character vector of lines.
.clean_lines <- function(lines) {
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0L]
  lines <- lines[!startsWith(lines, ";")]   # FMP comment character
  lines
}

# Shared GISINFO parser: reads all entries from the GISINFO block and
# deduplicates by label, keeping the highest-quality entry per label.
# Quality ranking: non-zero GIS coords > non-zero screen coords > all-zero.
# This prevents earlier all-zero RIVER SECTION entries from masking later
# entries for the same label that carry real coordinates (e.g. BRIDGE or QTBDY).
.parse_gisinfo_raw <- function(lines) {
  gi_start <- which(toupper(trimws(lines)) == "GISINFO")
  if (length(gi_start) == 0L) return(NULL)

  gi_lines <- lines[(gi_start[1L] + 1L):length(lines)]
  if (length(gi_lines) == 0L) return(NULL)

  results <- vector("list", length(gi_lines))
  for (i in seq_along(gi_lines)) {
    tokens <- strsplit(gi_lines[[i]], "\\s+")[[1L]]
    if (length(tokens) < 6L) next
    nums <- suppressWarnings(as.numeric(tail(tokens, 5L)))
    if (anyNA(nums)) next
    label <- tokens[length(tokens) - 5L]
    results[[i]] <- data.table(
      label    = label,
      x_screen = nums[1L], y_screen = nums[2L],
      x_gis    = nums[3L], y_gis    = nums[4L]
    )
  }

  gi <- rbindlist(Filter(Negate(is.null), results))
  if (nrow(gi) == 0L) return(NULL)

  gi[, quality := (x_gis != 0L | y_gis != 0L) * 2L + (x_screen != 0L | y_screen != 0L)]
  gi <- gi[gi[, .I[which.max(quality)], by = label]$V1]
  gi[, quality := NULL]
  gi
}

# Returns BNG GIS coordinates from GISINFO, or NULL when none are valid.
.extract_gisinfo_coords <- function(lines) {
  gi <- .parse_gisinfo_raw(lines)
  if (is.null(gi)) return(NULL)
  valid <- gi[x_gis > 0 & x_gis <= 800000 & y_gis > 0 & y_gis <= 1300000]
  if (nrow(valid) == 0L) return(NULL)
  valid[, .(label, chainage = 0, x = x_gis, y = y_gis)]
}

# Returns screen (schematic) coordinates from GISINFO for nodes where at
# least one screen coordinate is non-zero.  Used as a last resort for models
# with a drawn schematic but no real-world georeferencing.
.extract_gisinfo_screen_coords <- function(lines) {
  gi <- .parse_gisinfo_raw(lines)
  if (is.null(gi)) return(NULL)
  valid <- gi[x_screen != 0 | y_screen != 0]
  if (nrow(valid) == 0L) return(NULL)
  valid[, .(label, chainage = 0, x = x_screen, y = y_screen)]
}

# Parse RIVER SECTION labels from the DAT body and assign 1D positions using
# the chainage encoded in the label.  FMP convention: "m<n>" means negative
# chainage -n (e.g. "m60" → -60); plain numeric strings are used directly.
# Non-convertible labels (e.g. "BRIDU") are skipped.
# Used when a GISINFO block is present but carries no GIS coordinates.
.extract_section_chainage_coords <- function(lines) {
  body_end <- length(lines)
  for (kw in c("GISINFO", "INITIAL CONDITIONS")) {
    idx <- which(toupper(trimws(lines)) == kw)
    if (length(idx) > 0L) body_end <- min(body_end, idx[1L] - 1L)
  }
  if (body_end < 2L) return(NULL)
  body <- lines[seq_len(body_end)]

  # Lines whose first token is "SECTION" open a cross-section block.
  # The label is always on the very next line (a single whitespace-free token).
  sec_idx <- which(startsWith(toupper(body), "SECTION"))
  if (length(sec_idx) == 0L) return(NULL)

  lbl_idx <- sec_idx + 1L
  lbl_idx <- lbl_idx[lbl_idx <= length(body)]
  labels  <- unique(trimws(body[lbl_idx]))
  labels  <- labels[!grepl("\\s", labels)]
  if (length(labels) == 0L) return(NULL)

  to_ch <- function(lbl) {
    if (grepl("^m[0-9]", lbl)) return(-suppressWarnings(as.numeric(sub("^m", "", lbl))))
    suppressWarnings(as.numeric(lbl))
  }
  ch   <- vapply(labels, to_ch, numeric(1L), USE.NAMES = FALSE)
  keep <- !is.na(ch)
  if (!any(keep)) return(NULL)

  data.table(label = labels[keep], chainage = ch[keep], x = ch[keep], y = 0)
}

# Legacy coordinate heuristic for older DAT formats that lack a GISINFO block.
# Treats the last two numeric tokens on any line as BNG easting/northing.
.extract_legacy_coords <- function(lines) {
  coord_pattern <- "^(\\S+)\\s+(.*)"
  results <- lapply(lines, function(ln) {
    if (!grepl(coord_pattern, ln)) return(NULL)
    label  <- sub(coord_pattern, "\\1", ln)
    rest   <- sub(coord_pattern, "\\2", ln)
    tokens <- suppressWarnings(as.numeric(strsplit(trimws(rest), "\\s+")[[1L]]))
    tokens <- tokens[!is.na(tokens)]
    if (length(tokens) < 3L) return(NULL)
    x <- tokens[length(tokens) - 1L]
    y <- tokens[length(tokens)]
    if (is.na(x) || is.na(y))     return(NULL)
    if (x < 0    || x > 800000)   return(NULL)
    if (y < 0    || y > 1300000)  return(NULL)
    if (x < 1000 && y < 1000)     return(NULL)
    data.table(label = label, chainage = tokens[1L], x = x, y = y)
  })
  dt <- rbindlist(Filter(Negate(is.null), results))
  if (nrow(dt) == 0L) return(NULL)
  dt
}


# ---------------------------------------------------------------------------
# parse_gxy()
# ---------------------------------------------------------------------------
#
# GXY format (text):
#   [ReachLabel]
#   X1    Y1
#   X2    Y2
#   ...
#   [NextReachLabel]
#   ...
#
# Reach labels are lines enclosed in square brackets.
# Coordinate pairs follow until the next label or EOF.
#
# Returns an sf data frame with columns:
#   reach_id  (character)  -- label from the GXY file
#   geometry  (LINESTRING, EPSG:27700)
#
# A reach with fewer than 2 coordinate pairs is dropped with a warning.

parse_gxy <- function(path) {
  stopifnot(file.exists(path))
  cli_inform("Parsing GXY: {.path {path}}")

  raw   <- readLines(path, warn = FALSE)
  lines <- .clean_lines(raw)

  # Identify reach label lines
  is_label <- startsWith(lines, "[") & endsWith(lines, "]")

  if (!any(is_label)) {
    cli_abort("No reach labels found in {.path {path}}. Check file format.")
  }

  label_idx <- which(is_label)
  reach_ids <- gsub("^\\[|\\]$", "", lines[label_idx])

  # For each label, collect coordinate lines until the next label or EOF
  n_reaches <- length(label_idx)
  end_idx   <- c(label_idx[-1L] - 1L, length(lines))

  geoms <- vector("list", n_reaches)

  for (i in seq_len(n_reaches)) {
    coord_lines <- lines[(label_idx[i] + 1L):end_idx[i]]
    coord_lines <- coord_lines[!startsWith(coord_lines, "[")]  # safety

    if (length(coord_lines) < 2L) {
      cli_warn("Reach {.val {reach_ids[i]}} has fewer than 2 coordinate pairs; skipping.")
      geoms[[i]] <- NA
      next
    }

    coords <- do.call(rbind, strsplit(coord_lines, "\\s+"))
    coords <- matrix(as.numeric(coords[, 1L:2L]), ncol = 2L)

    if (anyNA(coords)) {
      cli_warn("Non-numeric coordinates in reach {.val {reach_ids[i]}}; skipping.")
      geoms[[i]] <- NA
      next
    }

    geoms[[i]] <- st_linestring(coords)
  }

  # Drop failed reaches
  keep      <- !is.na(geoms)
  reach_ids <- reach_ids[keep]
  geoms     <- geoms[keep]

  if (length(geoms) == 0L) {
    cli_abort("No valid reaches parsed from {.path {path}}.")
  }

  out <- st_sf(
    reach_id = reach_ids,
    geometry = st_sfc(geoms, crs = .bng)
  )

  cli_inform("Parsed {nrow(out)} reach(es) from GXY.")
  out
}


# ---------------------------------------------------------------------------
# Content-parsing helpers for parse_dat()
# ---------------------------------------------------------------------------

# Convert an FMP section label to a numeric chainage, or NA.
# "m60" → -60 (FMP negative-chainage convention), "700" → 700, "BRIDU" → NA.
.label_to_chainage <- function(lbl) {
  if (grepl("^m[0-9]", lbl)) return(-suppressWarnings(as.numeric(sub("^m", "", lbl))))
  ch <- suppressWarnings(as.numeric(lbl))
  if (!is.na(ch)) ch else NA_real_
}

# Parse one RIVER SECTION block starting at line i of body.
# Returns list(unit = data.table row or NULL, next_i = integer).
.parse_river_section_block <- function(body, i) {
  if (i + 4L > length(body))                            return(list(unit = NULL, next_i = i + 1L))
  if (toupper(trimws(body[i + 1L])) != "SECTION")       return(list(unit = NULL, next_i = i + 1L))
  label <- trimws(body[i + 2L])
  n_pts <- suppressWarnings(as.integer(strsplit(trimws(body[i + 4L]), "\\s+")[[1L]][1L]))
  if (is.na(n_pts) || n_pts < 1L)                       return(list(unit = NULL, next_i = i + 3L))
  data_end <- i + 4L + n_pts
  if (data_end > length(body))                           return(list(unit = NULL, next_i = i + 3L))
  xz_lines <- body[(i + 5L):data_end]
  xz <- tryCatch(
    do.call(rbind, lapply(xz_lines, function(ln)
      suppressWarnings(as.numeric(strsplit(trimws(ln), "\\s+")[[1L]][1:4])))),
    error = function(e) NULL
  )
  if (is.null(xz))                                       return(list(unit = NULL, next_i = data_end + 1L))
  xz_dt <- data.table(
    offset    = xz[, 1L], elevation = xz[, 2L],
    n_value   = xz[, 3L], panel     = xz[, 4L]
  )
  unit <- data.table(
    label     = label,
    node_type = "RIVER SECTION",
    chainage  = .label_to_chainage(label),
    data      = list(xz_dt)
  )
  list(unit = unit, next_i = data_end + 1L)
}

# Parse one QTBDY / QHBDY / HTBDY block starting at line i of body.
.parse_boundary_block <- function(body, i, kw) {
  if (i + 2L > length(body))                            return(list(unit = NULL, next_i = i + 1L))
  label <- trimws(body[i + 1L])
  n_pts <- suppressWarnings(as.integer(strsplit(trimws(body[i + 2L]), "\\s+")[[1L]][1L]))
  if (is.na(n_pts) || n_pts < 1L)                       return(list(unit = NULL, next_i = i + 2L))
  data_end <- i + 2L + n_pts
  if (data_end > length(body))                           return(list(unit = NULL, next_i = i + 2L))
  pair_lines <- body[(i + 3L):data_end]
  pairs <- tryCatch(
    do.call(rbind, lapply(pair_lines, function(ln)
      suppressWarnings(as.numeric(strsplit(trimws(ln), "\\s+")[[1L]][1:2])))),
    error = function(e) NULL
  )
  if (is.null(pairs))                                    return(list(unit = NULL, next_i = data_end + 1L))
  col2 <- if (kw == "QHBDY") "stage" else "time"
  dt <- data.table(flow = pairs[, 1L])
  dt[[col2]] <- pairs[, 2L]
  unit <- data.table(
    label     = label,
    node_type = kw,
    chainage  = NA_real_,
    data      = list(dt)
  )
  list(unit = unit, next_i = data_end + 1L)
}

# Parse one BRIDGE block starting at line i of body.
# Extracts the opening geometry only (x, z, width); pier and span detail is skipped.
.parse_bridge_block <- function(body, i) {
  if (i + 2L > length(body))                            return(list(unit = NULL, next_i = i + 1L))
  us_label    <- strsplit(trimws(body[i + 2L]), "\\s+")[[1L]][1L]
  search_end  <- min(i + 80L, length(body))
  aligned_pos <- which(toupper(trimws(body[(i + 3L):search_end])) == "ALIGNED")[1L]
  if (is.na(aligned_pos))                                return(list(unit = NULL, next_i = i + 3L))
  aligned_i <- i + 2L + aligned_pos
  if (aligned_i + 1L > length(body))                    return(list(unit = NULL, next_i = aligned_i))
  n_open <- suppressWarnings(
    as.integer(strsplit(trimws(body[aligned_i + 1L]), "\\s+")[[1L]][1L])
  )
  if (is.na(n_open) || n_open < 1L)                     return(list(unit = NULL, next_i = aligned_i + 2L))
  open_end <- aligned_i + 1L + n_open
  if (open_end > length(body))                           return(list(unit = NULL, next_i = aligned_i + 2L))
  open_lines <- body[(aligned_i + 2L):open_end]
  open <- tryCatch(
    do.call(rbind, lapply(open_lines, function(ln)
      suppressWarnings(as.numeric(strsplit(trimws(ln), "\\s+")[[1L]][1:3])))),
    error = function(e) NULL
  )
  if (is.null(open))                                     return(list(unit = NULL, next_i = open_end + 1L))
  open_dt    <- data.table(x = open[, 1L], z = open[, 2L], width = open[, 3L])
  end_search <- seq(open_end + 1L, min(open_end + 30L, length(body)))
  end_off    <- which(trimws(body[end_search]) == "0")[1L]
  next_i     <- if (!is.na(end_off)) end_search[end_off] + 1L else open_end + 1L
  unit <- data.table(
    label     = us_label,
    node_type = "BRIDGE",
    chainage  = NA_real_,
    data      = list(open_dt)
  )
  list(unit = unit, next_i = next_i)
}

# Scan the DAT body for recognised unit blocks and return a data.table with one
# row per unit.  Columns: label, node_type, chainage, data (list-column).
# The body is everything before the first GISINFO or INITIAL CONDITIONS section.
.parse_dat_content <- function(lines) {
  body_end <- length(lines)
  for (kw in c("GISINFO", "INITIAL CONDITIONS")) {
    idx <- which(toupper(trimws(lines)) == kw)
    if (length(idx) > 0L) body_end <- min(body_end, idx[1L] - 1L)
  }
  body    <- lines[seq_len(body_end)]
  results <- list()
  bnd_kws <- c("QTBDY", "QHBDY", "HTBDY")
  i       <- 1L
  while (i <= length(body)) {
    kw <- toupper(trimws(body[i]))
    if (kw == "RIVER") {
      r <- .parse_river_section_block(body, i)
      if (!is.null(r$unit)) results <- c(results, list(r$unit))
      i <- r$next_i
    } else if (kw %in% bnd_kws) {
      r <- .parse_boundary_block(body, i, kw)
      if (!is.null(r$unit)) results <- c(results, list(r$unit))
      i <- r$next_i
    } else if (kw == "BRIDGE") {
      r <- .parse_bridge_block(body, i)
      if (!is.null(r$unit)) results <- c(results, list(r$unit))
      i <- r$next_i
    } else {
      i <- i + 1L
    }
  }
  if (length(results) == 0L) return(NULL)
  rbindlist(results, fill = TRUE)
}


# ---------------------------------------------------------------------------
# parse_dat()
# ---------------------------------------------------------------------------
#
# DAT files are the main FMP network file.  Coordinates are extracted using
# a two-strategy approach:
#
#   Strategy 1a (GISINFO block, GIS coords) — used whenever the file contains
#   a GISINFO section with non-zero x_gis / y_gis values.  Each entry has:
#     <unit_keyword(s)> <label> <x_screen> <y_screen> <x_gis> <y_gis> <visible>
#   x_gis / y_gis are the real-world BNG coordinates assigned in FMP.
#   Output sf has CRS = EPSG:27700.
#
#   Strategy 1b (chainage schematic) — when GISINFO exists but has no GIS
#   coords, section labels are parsed from RIVER SECTION blocks in the DAT
#   body.  Labels matching "m<n>" (FMP negative-chainage convention) → -n;
#   plain numeric labels → their value.  Nodes are placed at (chainage, 0).
#   Output sf has CRS = NA; a warning is emitted.
#
#   Strategy 1c (GISINFO screen coords) — last resort when the above also
#   fails.  Uses x_screen / y_screen (GUI pixel positions) for nodes where
#   at least one is non-zero.  Output sf has CRS = NA; a warning is emitted.
#
#   Strategy 2 (legacy heuristic) — fallback for older DAT variants that pre-
#   date the GISINFO block.  Treats the last two numeric tokens on qualifying
#   lines as BNG easting / northing.  Only attempted when no GISINFO block is
#   found.
#
# Returns an sf data frame with columns:
#   label     (character)  -- section label
#   chainage  (numeric)    -- 0 when derived from GISINFO
#   x, y      (numeric)    -- BNG easting / northing
#   geometry  (POINT, EPSG:27700)
#
# If as_lines = TRUE nodes are connected into LINESTRING per reach using
# label-prefix heuristics (labels like "REACH1_001", "REACH1_002" are
# grouped by the prefix before the last "_" or numeric suffix).

parse_dat <- function(path, as_lines = FALSE) {
  stopifnot(file.exists(path))
  cli_inform("Parsing DAT: {.path {path}}")

  raw   <- readLines(path, warn = FALSE)
  lines <- .clean_lines(raw)

  # Default: return hydraulic unit content as a data.table with a list-column.
  # The spatial extraction strategies below are only needed for as_lines = TRUE.
  if (!as_lines) {
    out <- .parse_dat_content(lines)
    if (is.null(out) || nrow(out) == 0L) {
      cli_abort("No unit blocks could be parsed from {.path {path}}.")
    }
    cli_inform("Parsed {nrow(out)} hydraulic unit{?s} from DAT.")
    return(out)
  }

  # --- Spatial extraction (as_lines = TRUE) ---
  # Used by model_geometry() as a fallback when no GXY file is available.

  has_gisinfo <- any(toupper(trimws(lines)) == "GISINFO")
  use_crs     <- .bng

  # Strategy 1: GISINFO GIS coordinates (real-world BNG).
  dt <- .extract_gisinfo_coords(lines)

  # Strategy 2: 1D chainage schematic from RIVER SECTION labels.
  # Preferred over screen coords: covers all sections, not just boundary nodes,
  # and positions them meaningfully along the reach.
  if (is.null(dt) && has_gisinfo) {
    dt <- .extract_section_chainage_coords(lines)
    if (!is.null(dt)) {
      use_crs <- NA_integer_
      cli_warn(c(
        "!" = "DAT has no GIS coordinates; placing sections at chainage positions (1D schematic).",
        "i" = "Geometry will have no CRS. Georeference the model in Flood Modeller Pro for BNG output."
      ))
    }
  }

  # Strategy 3: GISINFO screen coordinates (schematic pixel positions).
  if (is.null(dt) && has_gisinfo) {
    dt <- .extract_gisinfo_screen_coords(lines)
    if (!is.null(dt)) {
      use_crs <- NA_integer_
      cli_warn(c(
        "!" = "DAT has no GIS coordinates; using schematic screen positions instead.",
        "i" = "Geometry will have no CRS. Georeference the model in Flood Modeller Pro for BNG output."
      ))
    }
  }

  # Strategy 4: legacy heuristic for older DAT formats that lack a GISINFO block.
  if (is.null(dt) && !has_gisinfo) {
    dt <- .extract_legacy_coords(lines)
  }

  if (is.null(dt) || nrow(dt) == 0L) {
    cli_abort(
      "No coordinate records parsed from {.path {path}}. The DAT may use an unsupported format."
    )
  }

  # Remove duplicate label+chainage combinations (DAT can repeat headers)
  dt <- unique(dt, by = c("label", "chainage"))
  setorder(dt, label, chainage)

  cli_inform("Parsed {nrow(dt)} node(s) from DAT.")

  # --- Connect nodes into lines per reach ---
  # Group labels into a reach only when 2+ labels in dt share the same stripped
  # prefix. A sole holder of a prefix keeps its original label as group name,
  # avoiding spurious groups like "m" (from "m60") or "" (from "700").
  stripped      <- sub("_?\\d+$", "", dt$label)
  prefix_counts <- table(stripped)
  dt[, reach_group := {
    s <- sub("_?\\d+$", "", label)
    ifelse(prefix_counts[s] >= 2L, s, label)
  }]

  groups <- split(dt, dt$reach_group)

  geoms      <- vector("list", length(groups))
  group_ids  <- names(groups)
  n_points   <- 0L

  for (i in seq_along(groups)) {
    g <- groups[[i]]
    if (nrow(g) < 2L) {
      n_points   <- n_points + 1L
      geoms[[i]] <- st_point(c(g$x[1L], g$y[1L]))
      next
    }
    coords     <- as.matrix(g[, .(x, y)])
    geoms[[i]] <- st_linestring(coords)
  }

  out <- st_sf(
    reach_id = group_ids,
    geometry = st_sfc(geoms, crs = use_crs)
  )

  n_lines <- length(groups) - n_points
  if (n_lines > 0L && n_points > 0L) {
    cli_inform("Parsed {n_lines} reach line{?s} and {n_points} isolated point{?s} from DAT.")
  } else if (n_lines > 0L) {
    cli_inform("Parsed {n_lines} reach line{?s} from DAT.")
  } else {
    cli_inform("Parsed {n_points} node{?s} from DAT (no multi-node reaches; returned as points).")
  }
  out
}


# ---------------------------------------------------------------------------
# model_geometry()
# ---------------------------------------------------------------------------
#
# Wrapper that applies the tiered fallback for a single model:
#   1. GXY present and parseable   -> LINESTRING from parse_gxy()
#   2. GXY absent / failed         -> parse_dat(as_lines = TRUE)
#   3. DAT absent / failed         -> NULL (caller handles MC centroid fallback)
#
# Arguments:
#   model_id  character  model identifier (attached to output)
#   gxy_path  character or NULL
#   dat_path  character or NULL
#
# Returns an sf data frame with a model_id column, or NULL if both fail.
# The 'source' column records which file was used: "gxy", "dat", or NA.

model_geometry <- function(model_id, gxy_path = NULL, dat_path = NULL) {
  stopifnot(is.character(model_id), length(model_id) == 1L)

  try_parse <- function(fn, ...) {
    tryCatch(fn(...), error = function(e) {
      cli_warn("Parser failed for model {.val {model_id}}: {conditionMessage(e)}")
      NULL
    })
  }

  # Attempt 1: GXY
  if (!is.null(gxy_path) && file.exists(gxy_path)) {
    result <- try_parse(parse_gxy, gxy_path)
    if (!is.null(result)) {
      result$model_id <- model_id
      result$source   <- "gxy"
      cli_inform("Model {.val {model_id}}: geometry from GXY.")
      return(result)
    }
  }

  # Attempt 2: DAT
  if (!is.null(dat_path) && file.exists(dat_path)) {
    result <- try_parse(parse_dat, dat_path, as_lines = TRUE)
    if (!is.null(result)) {
      result$model_id <- model_id
      result$source   <- "dat"
      cli_inform("Model {.val {model_id}}: geometry from DAT (fallback).")
      return(result)
    }
  }

  # Attempt 3: nothing
  cli_warn("Model {.val {model_id}}: no parseable spatial file found. Use MC centroid as fallback.")
  NULL
}


# ---------------------------------------------------------------------------
# batch_model_geometry()
# ---------------------------------------------------------------------------
#
# Apply model_geometry() across a data frame of models.
#
# Expects a data frame / data.table with columns:
#   model_id, gxy_path, dat_path
#
# Returns a single sf object combining all successfully parsed models,
# plus a summary data.table of outcomes.

batch_model_geometry <- function(models_dt) {
  stopifnot(
    is.data.frame(models_dt),
    all(c("model_id", "gxy_path", "dat_path") %in% names(models_dt))
  )

  results <- vector("list", nrow(models_dt))

  for (i in seq_len(nrow(models_dt))) {
    row <- models_dt[i, ]
    results[[i]] <- model_geometry(
      model_id = row$model_id,
      gxy_path = row$gxy_path,
      dat_path = row$dat_path
    )
  }

  parsed  <- Filter(Negate(is.null), results)
  failed  <- models_dt$model_id[vapply(results, is.null, logical(1L))]

  if (length(failed) > 0L) {
    cli_inform(c(
      "!" = "{length(failed)} model(s) returned no geometry:",
      "*" = paste(failed, collapse = ", ")
    ))
  }

  if (length(parsed) == 0L) {
    cli_abort("No model geometries were parsed successfully.")
  }

  combined <- do.call(rbind, parsed)
  cli_inform("Done. {nrow(combined)} feature(s) across {length(parsed)} model(s).")
  combined
}
