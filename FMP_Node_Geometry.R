# fmp_geometry.R
# Parsers for Flood Modeller Pro spatial files.
# Returns sf objects in EPSG:27700 (BNG) throughout.
#
# Functions exported:
#   parse_gxy()       -- parse a .gxy file -> LINESTRING sf per reach
#   parse_dat()       -- parse a .dat file -> POINT sf per node, optionally LINESTRING per reach
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
# parse_dat()
# ---------------------------------------------------------------------------
#
# DAT files are the main FMP network file. They contain many section types.
# This parser targets SECTION and NODE records that carry X/Y coordinates.
#
# FMP 1D DAT coordinate format:
#   Section header lines start with a label in columns 1-12 followed by
#   chainage, then X and Y as the last two numeric fields on the line.
#   The exact column layout varies by FMP version, so we use a
#   position-tolerant regex rather than fixed-width parsing.
#
# Specifically, lines that define a cross-section or interpolated section
# begin with a non-whitespace label and contain at least 4 numeric tokens.
# The convention is:
#   <label>  <chainage>  ...  <X>  <Y>
# where X and Y are the last two numeric values on the line.
#
# This is a best-effort parser. Some DAT variants (esp. 2D links, structures)
# do not follow this layout and are skipped.
#
# Returns an sf data frame with columns:
#   label     (character)  -- section label
#   chainage  (numeric)
#   x, y      (numeric)    -- BNG easting / northing
#   geometry  (POINT, EPSG:27700)
#
# If as_lines = TRUE, nodes are connected into LINESTRING per reach using
# label prefix heuristics (labels like "REACH1_001", "REACH1_002" are
# grouped by the prefix before the last "_" or numeric suffix).

parse_dat <- function(path, as_lines = FALSE) {
  stopifnot(file.exists(path))
  cli_inform("Parsing DAT: {.path {path}}")

  raw   <- readLines(path, warn = FALSE)
  lines <- .clean_lines(raw)

  # Regex: line starts with a non-space label, followed by >= 3 numeric tokens
  # Capture label, then extract all numeric tokens from the line.
  # We keep only lines where the last two tokens look like plausible BNG
  # coordinates (easting 100000-700000, northing 0-1300000).

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

    # Rough BNG bounds check
    if (is.na(x) || is.na(y))          return(NULL)
    if (x < 0 || x > 800000)           return(NULL)
    if (y < 0 || y > 1300000)          return(NULL)
    # Reject obviously wrong values (e.g. chainage parsed as coordinate)
    if (x < 1000 && y < 1000)          return(NULL)

    chainage <- tokens[1L]

    data.table(label = label, chainage = chainage, x = x, y = y)
  })

  dt <- rbindlist(Filter(Negate(is.null), results))

  if (nrow(dt) == 0L) {
    cli_abort("No coordinate records parsed from {.path {path}}. The DAT may use an unsupported format.")
  }

  # Remove duplicate label+chainage combinations (DAT can repeat headers)
  dt <- unique(dt, by = c("label", "chainage"))
  setorder(dt, label, chainage)

  cli_inform("Parsed {nrow(dt)} node(s) from DAT.")

  if (!as_lines) {
    out <- st_sf(
      dt[, .(label, chainage, x, y)],
      geometry = st_sfc(
        mapply(function(ex, ey) st_point(c(ex, ey)),
               dt$x, dt$y, SIMPLIFY = FALSE),
        crs = .bng
      )
    )
    return(out)
  }

  # --- Connect nodes into lines per reach ---
  # Group label by stripping trailing numeric suffix to get a reach group.
  # e.g. "AVON_001", "AVON_002" -> reach_group "AVON"
  dt[, reach_group := sub("_?\\d+$", "", label)]

  groups <- split(dt, dt$reach_group)

  geoms      <- vector("list", length(groups))
  group_ids  <- names(groups)

  for (i in seq_along(groups)) {
    g <- groups[[i]]
    if (nrow(g) < 2L) {
      cli_warn("Reach group {.val {group_ids[i]}} has only 1 node; returned as point.")
      geoms[[i]] <- st_point(c(g$x[1L], g$y[1L]))
      next
    }
    coords    <- as.matrix(g[, .(x, y)])
    geoms[[i]] <- st_linestring(coords)
  }

  out <- st_sf(
    reach_id = group_ids,
    geometry = st_sfc(geoms, crs = .bng)
  )

  cli_inform("Connected nodes into {nrow(out)} reach line(s).")
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
