# FMP_DAT.R
# R6 implementation of the Flood Modeller Pro DAT model API.
# Mirrors the Python floodmodeller-api DAT class.
#
# Exported:
#   DAT           -- R6 class: read, navigate, edit, and plot a .dat model
#   plot_section()       -- cross-section profile
#   plot_boundary()      -- boundary condition time-series
#   plot_long_section()  -- bed elevation long-section
#   plot_bridge()        -- bridge opening geometry
#   plot_network()       -- 1D network schematic
#
# Dependencies: R6, data.table, cli, ggplot2

library(R6)
library(data.table)
library(cli)
library(ggplot2)


# ---------------------------------------------------------------------------
# Constants — unit type registry
# ---------------------------------------------------------------------------

.UNIT_REGISTRY <- list(
  RIVER          = list(group = "sections",   has_subtype = TRUE),
  BRIDGE         = list(group = "structures", has_subtype = TRUE),
  WEIR           = list(group = "structures", has_subtype = TRUE),
  "FLAT-V WEIR"  = list(group = "structures", has_subtype = FALSE),
  SPILL          = list(group = "structures", has_subtype = FALSE),
  SLUICE         = list(group = "structures", has_subtype = TRUE),
  ORIFICE        = list(group = "structures", has_subtype = FALSE),
  CULVERT        = list(group = "structures", has_subtype = FALSE),
  CONDUIT        = list(group = "conduits",   has_subtype = TRUE),
  JUNCTION       = list(group = "connectors", has_subtype = FALSE),
  LATERAL        = list(group = "connectors", has_subtype = FALSE),
  REACH          = list(group = "connectors", has_subtype = FALSE),
  QTBDY          = list(group = "boundaries", has_subtype = FALSE),
  QHBDY          = list(group = "boundaries", has_subtype = FALSE),
  HTBDY          = list(group = "boundaries", has_subtype = FALSE),
  REFHBDY        = list(group = "boundaries", has_subtype = FALSE),
  FEHBDY         = list(group = "boundaries", has_subtype = FALSE),
  FRQSIM         = list(group = "boundaries", has_subtype = FALSE),
  FSRBDY         = list(group = "boundaries", has_subtype = FALSE),
  FSSR16BDY      = list(group = "boundaries", has_subtype = FALSE),
  GERRBDY        = list(group = "boundaries", has_subtype = FALSE),
  REBDY          = list(group = "boundaries", has_subtype = FALSE),
  REFH2BDY       = list(group = "boundaries", has_subtype = FALSE),
  SCSBDY         = list(group = "boundaries", has_subtype = FALSE),
  NCDBDY         = list(group = "boundaries", has_subtype = FALSE),
  TIDBDY         = list(group = "boundaries", has_subtype = FALSE),
  BLOCKAGE       = list(group = "losses",     has_subtype = FALSE),
  LOSS           = list(group = "losses",     has_subtype = FALSE),
  RESERVOIR      = list(group = "controls",   has_subtype = FALSE)
)

.SPECIAL_TYPES <- c("INITIAL CONDITIONS", "GISINFO", "COMMENT", "VARIABLES")
.ALL_UNIT_TYPES <- c(names(.UNIT_REGISTRY), .SPECIAL_TYPES)


# ---------------------------------------------------------------------------
# Low-level text helpers
# ---------------------------------------------------------------------------

.split_10_char <- function(line) {
  n <- nchar(line)
  if (n == 0L) return(character(0))
  starts <- seq(1L, n, by = 10L)
  trimws(substring(line, starts, pmin(starts + 9L, n)))
}

.to_float <- function(x, default = 0.0) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0L || is.na(v)) default else v
}

.to_int <- function(x, default = 0L) {
  v <- suppressWarnings(as.integer(x))
  if (length(v) == 0L || is.na(v)) default else v
}

.lbl <- function(line, n = 12L) trimws(substr(line, 1L, n))

.toks <- function(line) {
  t <- strsplit(trimws(line), "\\s+")[[1L]]
  t[nchar(t) > 0L]
}

`%||%` <- function(x, y) if (!is.null(x)) x else y


# ---------------------------------------------------------------------------
# Unit constructor
# All units are S3 lists with class c("<TYPE>_<SUBTYPE>", "FMPUnit").
# Fields:
#   name          character  unit label (NA for COMMENT)
#   unit          character  type keyword ("RIVER", "BRIDGE", etc.)
#   subtype       character  subtype or NA
#   dist_to_next  numeric    downstream distance; NA where not applicable
#   ds_label      character  explicit downstream label (structures); NA otherwise
#   labels        character  extra labels (JUNCTION connections)
#   all_labels    character  union of name + labels + ds_label
#   data          list       unit-specific data.tables
#   .raw_lines    character  original DAT lines for future write-back
# ---------------------------------------------------------------------------

.new_unit <- function(name, unit_type, subtype = NA_character_,
                      dist_to_next = NA_real_, ds_label = NA_character_,
                      labels = character(0), data = list(),
                      raw_lines = character(0)) {
  all_labels <- unique(c(
    if (!is.null(name) && !is.na(name)) name,
    labels,
    if (!is.null(ds_label) && !is.na(ds_label)) ds_label
  ))
  cls <- if (!is.na(subtype)) {
    c(paste0(unit_type, "_", gsub("[ -]", "_", subtype)), "FMPUnit")
  } else {
    c(unit_type, "FMPUnit")
  }
  structure(
    list(
      name         = name,
      unit         = unit_type,
      subtype      = subtype,
      dist_to_next = dist_to_next,
      ds_label     = ds_label,
      labels       = labels,
      all_labels   = all_labels,
      data         = data,
      .raw_lines   = raw_lines
    ),
    class = cls
  )
}

# ---------------------------------------------------------------------------
# Unit parsers
# Each function receives `lines` = the raw DAT lines for one block (1-indexed)
# and `n` = label_len. Returns an FMPUnit or NULL.
# ---------------------------------------------------------------------------

# RIVER SECTION
.parse_river_section <- function(lines, n = 12L) {
  label        <- .lbl(lines[3L], n)
  dist_to_next <- .to_float(.toks(lines[4L])[1L], 0.0)
  n_pts        <- .to_int(.toks(lines[5L])[1L])

  xz_dt <- data.table(offset = numeric(), elevation = numeric(),
                      n_value = numeric(), panel = numeric())
  if (n_pts > 0L && length(lines) >= 5L + n_pts) {
    rows <- lapply(lines[6L:(5L + n_pts)], function(ln) {
      v <- suppressWarnings(as.numeric(.toks(ln)))
      length(v) <- 4L
      v
    })
    m <- do.call(rbind, rows)
    xz_dt <- data.table(offset    = m[, 1L], elevation = m[, 2L],
                        n_value   = m[, 3L], panel     = m[, 4L])
  }

  .new_unit(label, "RIVER", "SECTION",
            dist_to_next = dist_to_next,
            data         = list(cross_section = xz_dt),
            raw_lines    = lines)
}

# RIVER FLOOD PLAIN (label + dist_to_next only; geometry is complex and model-specific)
.parse_river_flood_plain <- function(lines, n = 12L) {
  label        <- .lbl(lines[3L], n)
  dist_to_next <- .to_float(.toks(lines[4L])[1L], 0.0)
  .new_unit(label, "RIVER", "FLOOD PLAIN",
            dist_to_next = dist_to_next,
            data         = list(),
            raw_lines    = lines)
}

# Generic flow/time-series boundary: QTBDY, FSRBDY, FRQSIM, REBDY, etc.
.parse_flow_boundary <- function(lines, unit_type, n = 12L) {
  label <- .lbl(lines[2L], n)
  n_pts <- .to_int(.toks(lines[3L])[1L])
  ts_dt <- data.table(flow = numeric(), time = numeric())

  if (n_pts > 0L && length(lines) >= 3L + n_pts) {
    rows <- lapply(lines[4L:(3L + n_pts)], function(ln) {
      v <- suppressWarnings(as.numeric(.toks(ln)))
      v[1:2]
    })
    m     <- do.call(rbind, rows)
    ts_dt <- data.table(flow = m[, 1L], time = m[, 2L])
  }
  .new_unit(label, unit_type, data = list(time_series = ts_dt), raw_lines = lines)
}

# QHBDY — flow vs stage
.parse_qhbdy <- function(lines, n = 12L) {
  label <- .lbl(lines[2L], n)
  n_pts <- .to_int(.toks(lines[3L])[1L])
  ts_dt <- data.table(flow = numeric(), stage = numeric())

  if (n_pts > 0L && length(lines) >= 3L + n_pts) {
    rows <- lapply(lines[4L:(3L + n_pts)], function(ln) {
      v <- suppressWarnings(as.numeric(.toks(ln)))
      v[1:2]
    })
    m     <- do.call(rbind, rows)
    ts_dt <- data.table(flow = m[, 1L], stage = m[, 2L])
  }
  .new_unit(label, "QHBDY", data = list(time_series = ts_dt), raw_lines = lines)
}

# HTBDY — stage vs time
.parse_htbdy <- function(lines, n = 12L) {
  label <- .lbl(lines[2L], n)
  n_pts <- .to_int(.toks(lines[3L])[1L])
  ts_dt <- data.table(stage = numeric(), time = numeric())

  if (n_pts > 0L && length(lines) >= 3L + n_pts) {
    rows <- lapply(lines[4L:(3L + n_pts)], function(ln) {
      v <- suppressWarnings(as.numeric(.toks(ln)))
      v[1:2]
    })
    m     <- do.call(rbind, rows)
    ts_dt <- data.table(stage = m[, 1L], time = m[, 2L])
  }
  .new_unit(label, "HTBDY", data = list(time_series = ts_dt), raw_lines = lines)
}

# BRIDGE USBPR1978 — opening geometry only; pier/span detail stored as raw
.parse_bridge_usbpr1978 <- function(lines, n = 12L) {
  us_label <- .lbl(lines[3L], n)
  ds_label <- if (nchar(lines[3L]) > n) .lbl(substr(lines[3L], n + 1L, nchar(lines[3L])), n) else NA_character_

  aligned_i <- which(toupper(trimws(lines)) == "ALIGNED")[1L]
  open_dt   <- data.table(x = numeric(), soffit = numeric(), deck = numeric())

  if (!is.na(aligned_i) && aligned_i + 1L <= length(lines)) {
    n_open <- .to_int(.toks(lines[aligned_i + 1L])[1L])
    if (n_open > 0L && aligned_i + 1L + n_open <= length(lines)) {
      rows <- lapply(lines[(aligned_i + 2L):(aligned_i + 1L + n_open)], function(ln) {
        v <- suppressWarnings(as.numeric(.toks(ln)))
        v[1:3]
      })
      m       <- do.call(rbind, rows)
      # cols: x offset | soffit elevation | deck (road) elevation
      open_dt <- data.table(x = m[, 1L], soffit = m[, 2L], deck = m[, 3L])
    }
  }

  .new_unit(us_label, "BRIDGE", "USBPR1978",
            ds_label  = ds_label,
            data      = list(opening = open_dt),
            raw_lines = lines)
}

# JUNCTION — one or more labels packed at fixed label_len offsets
.parse_junction <- function(lines, n = 12L) {
  lbl_line <- lines[2L]
  lbls     <- character(0)
  pos      <- 1L
  while (pos <= nchar(lbl_line)) {
    chunk <- trimws(substr(lbl_line, pos, pos + n - 1L))
    if (nchar(chunk) > 0L) lbls <- c(lbls, chunk)
    pos <- pos + n
  }
  name <- if (length(lbls) > 0L) lbls[1L] else NA_character_
  .new_unit(name, "JUNCTION", labels = lbls, data = list(), raw_lines = lines)
}

# Generic fallback — extracts label and stores raw lines
.parse_generic_unit <- function(lines, unit_type, has_subtype = FALSE, n = 12L) {
  lbl_idx  <- if (has_subtype) 3L else 2L
  label    <- if (length(lines) >= lbl_idx) .lbl(lines[lbl_idx], n) else NA_character_
  subtype  <- if (has_subtype && length(lines) >= 2L) trimws(lines[2L]) else NA_character_
  .new_unit(label, unit_type, subtype = subtype, data = list(), raw_lines = lines)
}


# ---------------------------------------------------------------------------
# Initial conditions parser
# ---------------------------------------------------------------------------

.parse_initial_conditions <- function(lines) {
  if (length(lines) < 3L) return(data.table())
  data_lines <- lines[3L:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0L]

  rows <- lapply(data_lines, function(ln) {
    label <- trimws(substr(ln, 1L, 12L))
    rest  <- if (nchar(ln) > 12L) substr(ln, 13L, nchar(ln)) else ""
    vals  <- suppressWarnings(as.numeric(.split_10_char(rest)))
    list(label    = label,
         easting  = vals[1L], northing = vals[2L],
         flow     = vals[3L], stage    = vals[4L],
         froude   = vals[5L], velocity = vals[6L],
         umode    = vals[7L], ustate   = vals[8L],
         z        = vals[9L])
  })
  rbindlist(rows, fill = TRUE)
}


# ---------------------------------------------------------------------------
# General parameters parser
# ---------------------------------------------------------------------------

.parse_general_parameters <- function(raw_data) {
  title <- raw_data[1L]
  line2 <- sprintf("%-70s", if (length(raw_data) >= 3L) raw_data[3L] else "")
  p     <- .split_10_char(line2)
  if (length(p) < 7L || p[7L] == "") {
    p[7L] <- "DEFAULT"
  }
  line3 <- sprintf("%-70s", if (length(raw_data) >= 4L) raw_data[4L] else "")
  p     <- c(p, .split_10_char(line3))

  list(
    title                = title,
    node_count           = .to_int(p[1L],   0L),
    lower_froude         = .to_float(p[2L],  0.75),
    upper_froude         = .to_float(p[3L],  0.9),
    min_depth            = .to_float(p[4L],  0.1),
    convergence_direct   = .to_float(p[5L],  0.001),
    label_len            = .to_int(p[6L],   12L),
    units                = p[7L],
    water_temperature    = .to_float(p[8L],  10.0),
    convergence_flow     = .to_float(p[9L],  0.01),
    convergence_head     = .to_float(p[10L], 0.01),
    mathematical_damping = .to_float(p[11L], 0.7),
    pivotal_choice       = .to_float(p[12L], 0.1),
    under_relaxation     = .to_float(p[13L], 0.7),
    matrix_dummy         = .to_float(p[14L], 0.0),
    rad_file             = if (length(raw_data) >= 6L) raw_data[6L] else ""
  )
}


# ---------------------------------------------------------------------------
# DAT R6 class
# ---------------------------------------------------------------------------

DAT <- R6Class(
  "DAT",
  cloneable = FALSE,

  # ------------------------------------------------------------------
  # Public fields and methods
  # ------------------------------------------------------------------
  public = list(

    filepath           = NULL,
    title              = NULL,
    general_parameters = NULL,
    initial_conditions = NULL,

    sections   = NULL,   # named list of RIVER_SECTION / RIVER_FLOOD_PLAIN units
    boundaries = NULL,   # named list of QTBDY / QHBDY / HTBDY / … units
    structures = NULL,   # named list of BRIDGE / WEIR / … units
    conduits   = NULL,
    losses     = NULL,
    connectors = NULL,   # named list of JUNCTION / LATERAL units
    controls   = NULL,

    # ---- Initialise ------------------------------------------------
    initialize = function(filepath = NULL) {
      self$sections    <- list()
      self$boundaries  <- list()
      self$structures  <- list()
      self$conduits    <- list()
      self$losses      <- list()
      self$connectors  <- list()
      self$controls    <- list()
      private$.unsupported <- list()
      private$.all_units   <- list()

      if (!is.null(filepath)) {
        self$filepath <- normalizePath(as.character(filepath), mustWork = TRUE)
        private$.read()
      }
    },

    # ---- Print summary ---------------------------------------------
    print = function(...) {
      cli_h1("Flood Modeller DAT")
      if (!is.null(self$filepath))
        cli_inform(c("i" = "File: {.path {self$filepath}}"))
      if (!is.null(self$title) && nchar(self$title) > 0L)
        cli_inform(c("i" = "Title: {self$title}"))
      if (!is.null(self$general_parameters))
        cli_inform(c("i" = "Units: {self$general_parameters$units}  |  Nodes: {self$general_parameters$node_count}"))
      cli_inform(c(
        "i" = "Sections:    {length(self$sections)}",
        "i" = "Boundaries:  {length(self$boundaries)}",
        "i" = "Structures:  {length(self$structures)}",
        "i" = "Conduits:    {length(self$conduits)}",
        "i" = "Connectors:  {length(self$connectors)}",
        "i" = "Losses:      {length(self$losses)}",
        "i" = "Controls:    {length(self$controls)}",
        "i" = "Unsupported: {length(private$.unsupported)}"
      ))
      invisible(self)
    },

    # ---- all_units active property ---------------------------------
    all_units = function() private$.all_units,

    # ---- Network navigation ----------------------------------------

    #' Find the unit immediately downstream of `unit`.
    next_unit = function(unit) {
      stopifnot(inherits(unit, "FMPUnit"))

      if (!is.null(unit$dist_to_next) && !is.na(unit$dist_to_next)) {
        if (unit$dist_to_next != 0)
          return(private$.next_in_dat(unit))
        return(private$.name_match(unit))
      }

      if (!is.null(unit$ds_label) && !is.na(unit$ds_label))
        return(private$.name_match(unit, name_override = unit$ds_label))

      if (unit$unit == "JUNCTION")
        return(lapply(unit$labels, function(l) private$.name_match(unit, l)))

      if (unit$unit %in% c("QHBDY", "NCDBDY", "TIDBDY"))
        return(NULL)

      private$.name_match(unit)
    },

    #' Find the unit immediately upstream of `unit`.
    prev_unit = function(unit) {
      stopifnot(inherits(unit, "FMPUnit"))

      inflow_types <- c("QTBDY", "HTBDY", "REFHBDY", "FEHBDY", "FRQSIM",
                        "FSRBDY", "FSSR16BDY", "GERRBDY", "REBDY",
                        "REFH2BDY", "SCSBDY")
      if (unit$unit %in% inflow_types) return(NULL)

      if (unit$unit == "JUNCTION")
        return(lapply(unit$labels, function(l) private$.name_match(unit, l)))

      prev_units <- list()
      prev_dat   <- private$.prev_in_dat(unit)
      nm_match   <- private$.name_match(unit)
      ds_match   <- private$.ds_label_match(unit)
      jnc_match  <- Filter(
        function(u) u$unit == "JUNCTION" && unit$name %in% u$labels,
        private$.all_units
      )

      if (!is.null(prev_dat) &&
          !is.null(prev_dat$dist_to_next) &&
          !is.na(prev_dat$dist_to_next) &&
          prev_dat$dist_to_next != 0) {
        prev_units <- c(prev_units, list(prev_dat))
        nm_match   <- NULL
      }

      for (m in list(nm_match, ds_match)) {
        if (is.null(m)) next
        if (inherits(m, "FMPUnit")) prev_units <- c(prev_units, list(m))
        else if (is.list(m))        prev_units <- c(prev_units, m)
      }
      prev_units <- c(prev_units, jnc_match)

      if (length(prev_units) == 0L) return(NULL)
      if (length(prev_units) == 1L) return(prev_units[[1L]])
      prev_units
    },

    # ---- Insert / remove -------------------------------------------

    #' Insert `unit` into the network.
    #' Exactly one of add_before, add_after, or add_at must be supplied.
    insert_unit = function(unit,
                           add_before = NULL,
                           add_after  = NULL,
                           add_at     = NULL) {
      stopifnot(inherits(unit, "FMPUnit"))
      n_args <- sum(!vapply(list(add_before, add_after, add_at), is.null, logical(1L)))
      if (n_args == 0L)
        stop("Provide add_before, add_after, or add_at.", call. = FALSE)
      if (n_args > 1L)
        stop("Only one positional argument is allowed.", call. = FALSE)

      if (unit$unit != "COMMENT") {
        grp <- private$.get_group(unit)
        if (!is.null(unit$name) && !is.na(unit$name) && unit$name %in% names(grp))
          stop(sprintf("Label '%s' already exists in %s.",
                       unit$name, private$.group_name(unit)), call. = FALSE)
      }

      idx <- private$.insert_index(add_before, add_after, add_at)
      private$.all_units <- append(private$.all_units, list(unit), after = idx)

      if (unit$unit != "COMMENT" && !is.null(unit$name) && !is.na(unit$name)) {
        grp <- private$.get_group(unit)
        grp[[unit$name]] <- unit
        private$.set_group(unit, grp)

        if (!unit$name %in% (self$initial_conditions$label %||% character(0))) {
          new_row <- data.table(
            label = unit$name, easting = NA_real_, northing = NA_real_,
            flow = 0, stage = 0, froude = 0, velocity = 0,
            umode = 0, ustate = 0, z = 0
          )
          self$initial_conditions <- rbindlist(
            list(self$initial_conditions %||% data.table(), new_row), fill = TRUE
          )
          self$general_parameters$node_count <-
            (self$general_parameters$node_count %||% 0L) + 1L
        }
      }
      invisible(self)
    },

    #' Remove `unit` from the network.
    remove_unit = function(unit) {
      stopifnot(inherits(unit, "FMPUnit"))

      idx <- Position(function(u) identical(u, unit), private$.all_units)
      if (is.null(idx))
        stop("Unit not found in DAT.", call. = FALSE)
      private$.all_units[[idx]] <- NULL

      if (unit$unit != "COMMENT" && !is.null(unit$name) && !is.na(unit$name)) {
        grp <- private$.get_group(unit)
        grp[[unit$name]] <- NULL
        private$.set_group(unit, grp)

        all_lbls <- unique(unlist(lapply(private$.all_units, `[[`, "all_labels")))
        if (!unit$name %in% all_lbls) {
          if (!is.null(self$initial_conditions))
            self$initial_conditions <-
              self$initial_conditions[label != unit$name]
          self$general_parameters$node_count <-
            max(0L, (self$general_parameters$node_count %||% 1L) - 1L)
        }
      }
      invisible(self)
    },

    # ---- Network graph ---------------------------------------------

    #' Build a directed network from the model.
    #' Returns list(nodes = list[FMPUnit], edges = list[list(from, to)]).
    get_network = function() {
      units <- Filter(function(u) u$unit != "COMMENT", private$.all_units)
      n     <- length(units)
      lbl2u <- list()

      # Mutate local copies so all_labels can be extended with dummy labels
      lbls_list <- lapply(units, function(u) u$all_labels)

      for (idx in seq_len(n)) {
        u        <- units[[idx]]
        in_reach <- !is.null(u$dist_to_next) && !is.na(u$dist_to_next) &&
                    u$dist_to_next > 0 && idx < n

        if (in_reach) {
          nxt <- units[[idx + 1L]]
          if (is.null(nxt$name) || is.na(nxt$name))
            stop("Reached a unit with no name during network build.", call. = FALSE)

          end_of_reach <- is.null(nxt$dist_to_next) || is.na(nxt$dist_to_next) ||
                          nxt$dist_to_next == 0 ||
                          idx + 1L >= n ||
                          is.null(units[[idx + 2L]]$dist_to_next) ||
                          is.na(units[[idx + 2L]]$dist_to_next)

          dummy <- if (end_of_reach) paste0(nxt$name, "_dummy") else nxt$name
          lbls_list[[idx]] <- c(lbls_list[[idx]], dummy)
          if (end_of_reach)
            lbls_list[[idx + 1L]] <- c(lbls_list[[idx + 1L]], dummy)
        }

        for (lbl in lbls_list[[idx]]) {
          lbl2u[[lbl]] <- c(lbl2u[[lbl]], list(u))
        }
      }

      invalid <- names(Filter(function(v) length(v) != 2L, lbl2u))
      if (length(invalid) > 0L)
        cli_warn(c(
          "!" = "{length(invalid)}/{length(lbl2u)} label(s) don't join exactly 2 units.",
          "i" = "Network may be disconnected: {.val {head(invalid, 5L)}}"
        ))

      valid <- Filter(function(v) length(v) == 2L, lbl2u)
      list(
        nodes = units,
        edges = lapply(valid, function(p) list(from = p[[1L]], to = p[[2L]]))
      )
    },

    # ---- Diff -------------------------------------------------------

    diff = function(other, force_print = FALSE) {
      stopifnot(inherits(other, "DAT"))
      diffs  <- character(0)
      max_show <- if (force_print) Inf else 25L

      chk <- function(a, b, label) {
        if (!identical(a, b)) diffs <<- c(diffs, label)
      }

      chk(self$title,             other$title,             "title")
      chk(self$general_parameters, other$general_parameters, "general_parameters")
      chk(sort(names(self$sections)),   sort(names(other$sections)),   "section labels")
      chk(sort(names(self$boundaries)), sort(names(other$boundaries)), "boundary labels")
      chk(sort(names(self$structures)), sort(names(other$structures)), "structure labels")

      for (nm in intersect(names(self$sections), names(other$sections)))
        chk(self$sections[[nm]]$data, other$sections[[nm]]$data,
            sprintf("sections[['%s']]$data", nm))
      for (nm in intersect(names(self$boundaries), names(other$boundaries)))
        chk(self$boundaries[[nm]]$data, other$boundaries[[nm]]$data,
            sprintf("boundaries[['%s']]$data", nm))

      if (length(diffs) == 0L) {
        cli_inform("DAT files are equivalent.")
        return(invisible(NULL))
      }

      cli_inform(c("!" = "{length(diffs)} difference(s) found:"))
      for (d in head(diffs, max_show)) cli_inform(c(" " = d))
      if (length(diffs) > max_show)
        cli_inform(c("i" = "... and {length(diffs) - max_show} more. Use force_print = TRUE to see all."))

      invisible(diffs)
    },

    # ---- ggplot2 plots ---------------------------------------------

    #' XZ cross-section profile for a named river section.
    plot_section = function(label) {
      u <- self$sections[[label]]
      if (is.null(u))
        stop(sprintf("Section '%s' not found.", label), call. = FALSE)
      xz <- u$data$cross_section
      if (is.null(xz) || nrow(xz) == 0L)
        stop("No cross-section data for this unit.", call. = FALSE)

      ggplot(xz, aes(x = offset, y = elevation)) +
        geom_ribbon(aes(ymin = min(elevation), ymax = elevation),
                    fill = "lightblue", alpha = 0.4) +
        geom_line(colour = "steelblue", linewidth = 0.9) +
        geom_point(colour = "steelblue", size = 1.8) +
        labs(title = sprintf("Cross-section: %s", label),
             x = "Offset (m)", y = "Elevation (mAOD)") +
        theme_minimal(base_size = 12)
    },

    #' Flow or stage boundary condition time-series.
    plot_boundary = function(label) {
      u <- self$boundaries[[label]]
      if (is.null(u))
        stop(sprintf("Boundary '%s' not found.", label), call. = FALSE)
      ts <- u$data$time_series
      if (is.null(ts) || nrow(ts) == 0L)
        stop("No time-series data for this unit.", call. = FALSE)

      y_col <- setdiff(names(ts), "time")[1L]
      x_col <- if ("time" %in% names(ts)) "time" else names(ts)[setdiff(seq_along(names(ts)), 1L)[1L]]
      y_lab <- switch(y_col, flow = "Flow (m³/s)", stage = "Stage (mAOD)", y_col)

      ggplot(ts, aes(x = .data[[x_col]], y = .data[[y_col]])) +
        geom_line(colour = "coral", linewidth = 0.9) +
        geom_point(colour = "coral", size = 1.8) +
        labs(title  = sprintf("Boundary: %s (%s)", label, u$unit),
             x = "Time (hours)", y = y_lab) +
        theme_minimal(base_size = 12)
    },

    #' Bed elevation long-section across all river sections.
    plot_long_section = function() {
      if (length(self$sections) == 0L)
        stop("No sections to plot.", call. = FALSE)

      to_ch <- function(lbl) {
        if (grepl("^m[0-9]", lbl)) return(-suppressWarnings(as.numeric(sub("^m", "", lbl))))
        suppressWarnings(as.numeric(lbl))
      }

      bed <- rbindlist(lapply(names(self$sections), function(nm) {
        xz <- self$sections[[nm]]$data$cross_section
        if (is.null(xz) || nrow(xz) == 0L) return(NULL)
        data.table(label    = nm,
                   chainage = to_ch(nm),
                   bed      = min(xz$elevation, na.rm = TRUE),
                   max_bank = max(xz$elevation, na.rm = TRUE))
      }), fill = TRUE)

      bed <- bed[!is.na(chainage)]
      if (nrow(bed) == 0L)
        stop("No sections have numeric chainages; cannot plot long section.", call. = FALSE)
      setorder(bed, chainage)

      ggplot(bed, aes(x = chainage)) +
        geom_ribbon(aes(ymin = bed, ymax = max_bank),
                    fill = "steelblue", alpha = 0.15) +
        geom_line(aes(y = bed),      colour = "steelblue",  linewidth = 0.9) +
        geom_line(aes(y = max_bank), colour = "grey60", linewidth = 0.5, linetype = "dashed") +
        geom_point(aes(y = bed), colour = "steelblue", size = 2) +
        labs(title = "Long section — bed elevation",
             x = "Chainage (m)", y = "Elevation (mAOD)") +
        theme_minimal(base_size = 12)
    },

    #' Bridge cross-section: soffit, deck, and adjacent river cross-section.
    plot_bridge = function(label) {
      u <- self$structures[[label]]
      if (is.null(u))
        stop(sprintf("Structure '%s' not found.", label), call. = FALSE)
      op <- u$data$opening
      if (is.null(op) || nrow(op) == 0L)
        stop("No opening geometry for this unit.", call. = FALSE)

      x_vals  <- op$x
      soffit  <- op$soffit

      # deck col: use parsed value when it is consistently above the soffit,
      # otherwise fall back to a 1.5 m headroom above the highest abutment.
      deck_raw <- op$deck
      use_deck <- !all(is.na(deck_raw)) && all(deck_raw >= soffit - 0.01, na.rm = TRUE)
      deck <- if (use_deck) deck_raw else rep(max(soffit, na.rm = TRUE) + 1.5, length(x_vals))

      # Look for the nearest adjacent RIVER SECTION to show as channel context.
      adj <- private$.find_adjacent_section(u)

      p <- ggplot()

      # Layer 1: adjacent cross-section (grey fill = earthwork, line = bed)
      if (!is.null(adj)) {
        xs  <- adj$data$cross_section
        # Centre-align the cross-section x range to the bridge x range
        xs_c   <- mean(range(xs$offset))
        br_c   <- mean(range(x_vals))
        xs_adj <- data.table(x = xs$offset - xs_c + br_c, y = xs$elevation)
        y_min  <- min(soffit, na.rm = TRUE) - 2.0
        p <- p +
          geom_ribbon(data = xs_adj,
                      aes(x = x, ymin = y_min, ymax = y),
                      fill = "#c8a96e", alpha = 0.45) +
          geom_line(data = xs_adj, aes(x = x, y = y),
                    colour = "#7a5c2e", linewidth = 0.8) +
          annotate("text",
                   x    = min(xs_adj$x),
                   y    = xs_adj$y[which.min(xs_adj$x)] + 0.15,
                   label = paste("XS:", adj$name),
                   hjust = 0, size = 3, colour = "#7a5c2e")
      }

      # Layer 2: bridge deck polygon (solid structure between soffit and deck)
      deck_poly <- data.table(
        x = c(x_vals,       rev(x_vals)),
        y = c(soffit,       rev(deck))
      )
      p <- p +
        geom_polygon(data = deck_poly, aes(x = x, y = y),
                     fill = "grey55", colour = "grey25", linewidth = 0.8)

      # Layer 3: soffit line (underside of bridge / clearance limit)
      p <- p +
        geom_line(data = data.table(x = x_vals, y = soffit),
                  aes(x = x, y = y),
                  colour = "black", linewidth = 0.6, linetype = "dashed") +
        annotate("text",
                 x     = mean(range(x_vals)),
                 y     = min(soffit, na.rm = TRUE) - 0.15,
                 label = sprintf("Soffit: %.2f mAOD", min(soffit, na.rm = TRUE)),
                 vjust = 1, size = 3.2, colour = "black")

      # Layer 4: deck top line + label
      p <- p +
        geom_line(data = data.table(x = x_vals, y = deck),
                  aes(x = x, y = y),
                  colour = "grey20", linewidth = 0.7) +
        annotate("text",
                 x     = max(x_vals),
                 y     = mean(deck, na.rm = TRUE),
                 label = sprintf("Deck: %.2f mAOD", mean(deck, na.rm = TRUE)),
                 hjust = -0.05, size = 3.2, colour = "grey20")

      ds <- if (!is.null(u$ds_label) && !is.na(u$ds_label))
              sprintf(" → %s", u$ds_label) else ""
      p +
        labs(title = sprintf("Bridge: %s%s", label, ds),
             x     = "Horizontal offset (m)",
             y     = "Elevation (mAOD)") +
        theme_minimal(base_size = 12) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(5, 40, 5, 5))
    },

    #' Schematic 1D network — all units plotted at their chainage position.
    plot_network = function() {
      to_ch <- function(lbl) {
        if (is.null(lbl) || is.na(lbl)) return(NA_real_)
        if (grepl("^m[0-9]", lbl)) return(-suppressWarnings(as.numeric(sub("^m", "", lbl))))
        suppressWarnings(as.numeric(lbl))
      }

      nodes <- rbindlist(lapply(private$.all_units, function(u) {
        if (u$unit == "COMMENT") return(NULL)
        ch <- to_ch(u$name)
        data.table(label     = u$name %||% NA_character_,
                   unit_type = u$unit,
                   chainage  = ch)
      }), fill = TRUE)

      nodes <- nodes[!is.na(chainage) & !is.na(label)]
      if (nrow(nodes) == 0L)
        stop("No units have numeric chainages; cannot plot network schematic.", call. = FALSE)
      setorder(nodes, chainage)

      # Colour palette by group
      type_colours <- c(
        RIVER    = "steelblue",
        BRIDGE   = "saddlebrown",
        WEIR     = "darkorange",
        QTBDY    = "forestgreen",
        QHBDY    = "forestgreen",
        HTBDY    = "forestgreen",
        JUNCTION = "purple"
      )

      ggplot(nodes, aes(x = chainage, y = 0,
                        colour = unit_type, label = label)) +
        geom_hline(yintercept = 0, colour = "grey80") +
        geom_point(size = 3.5) +
        ggrepel_text_if_installed(nodes) +
        scale_colour_manual(values = type_colours, na.value = "grey50") +
        scale_y_continuous(limits = c(-0.5, 1.2), breaks = NULL) +
        labs(title  = "Network schematic",
             x      = "Chainage (m)",
             y      = NULL,
             colour = "Unit type") +
        theme_minimal(base_size = 12) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
    }

  ),  # end public

  # ------------------------------------------------------------------
  # Private fields and methods
  # ------------------------------------------------------------------
  private = list(
    .raw_data    = NULL,
    .dat_struct  = NULL,
    .label_len   = 12L,
    .gxy_data    = NULL,
    .gxy_filepath = NULL,
    .all_units   = NULL,
    .unsupported = NULL,

    # ---- File read --------------------------------------------------
    .read = function() {
      private$.raw_data <- readLines(self$filepath, warn = FALSE)

      gxy_path <- sub("(?i)\\.dat$", ".gxy", self$filepath, perl = TRUE)
      if (file.exists(gxy_path)) {
        private$.gxy_filepath <- gxy_path
        private$.gxy_data     <- readLines(gxy_path, warn = FALSE)
      }

      private$.update_dat_struct()
      private$.get_general_parameters()
      private$.get_unit_definitions()

      if (!is.null(private$.gxy_data))
        private$.get_unit_locations()
    },

    # ---- DAT structure parse ----------------------------------------
    .update_dat_struct = function() {
      private$.dat_struct <- list()
      raw         <- private$.raw_data
      n_lines     <- length(raw)
      in_general  <- TRUE
      in_block    <- FALSE
      in_comment  <- FALSE
      comment_n   <- NULL
      gisinfo     <- FALSE
      gen_block   <- list(start = 1L, Type = "GENERAL")
      cur_block   <- list()

      close <- function(type, idx) {
        if (isTRUE(in_block)) {
          cur_block$end <<- idx - 1L
          private$.dat_struct <<- c(private$.dat_struct, list(cur_block))
          cur_block <<- list()
        }
        in_block    <<- TRUE
        cur_block$Type  <<- type
        cur_block$start <<- idx
      }

      for (idx in seq_len(n_lines)) {
        line <- raw[idx]
        ln   <- trimws(line)

        if (in_general) {
          if (ln == "END GENERAL") {
            gen_block$end <- idx
            private$.dat_struct <- c(private$.dat_struct, list(gen_block))
            in_general <- FALSE
          }
          next
        }

        if (in_comment) {
          if (is.null(comment_n)) {
            comment_n <- suppressWarnings(as.integer(ln))
            if (is.na(comment_n)) comment_n <- 0L
          } else {
            comment_n <- comment_n - 1L
            if (comment_n <= 0L) {
              cur_block$end <- idx + comment_n
              private$.dat_struct <- c(private$.dat_struct, list(cur_block))
              cur_block  <- list()
              in_block   <- FALSE
              in_comment <- FALSE
              comment_n  <- NULL
            }
          }
          next
        }

        if (ln == "COMMENT") {
          in_comment <- TRUE
          close("COMMENT", idx)
          next
        }

        if (ln == "GISINFO") {
          gisinfo <- TRUE
          close("GISINFO", idx)
          next
        }

        if (!gisinfo) {
          ut <- private$.identify_unit_type(ln)
          if (!is.null(ut)) close(ut, idx)
        }
      }

      if (length(cur_block) > 0L) {
        cur_block$end <- n_lines
        private$.dat_struct <- c(private$.dat_struct, list(cur_block))
      }
    },

    .identify_unit_type = function(ln) {
      if (nchar(ln) == 0L) return(NULL)
      parts <- .toks(ln)
      kw1 <- parts[1L]
      kw2 <- if (length(parts) >= 2L) paste(parts[1:2], collapse = " ") else ""
      if (!is.na(kw2) && kw2 %in% .ALL_UNIT_TYPES) return(kw2)
      if (!is.na(kw1) && kw1 %in% .ALL_UNIT_TYPES) return(kw1)
      NULL
    },

    # ---- General parameters ----------------------------------------
    .get_general_parameters = function() {
      gp <- .parse_general_parameters(private$.raw_data)
      self$title             <- gp$title
      private$.label_len     <- gp$label_len
      self$general_parameters <- gp
    },

    # ---- Unit definitions ------------------------------------------
    .get_unit_definitions = function() {
      for (blk in private$.dat_struct) {
        ut    <- blk$Type
        lines <- private$.raw_data[blk$start:blk$end]

        if (ut %in% c("GENERAL", "GISINFO")) next

        if (ut == "INITIAL CONDITIONS") {
          self$initial_conditions <- .parse_initial_conditions(lines)
          next
        }

        if (ut %in% c("COMMENT", "VARIABLES")) {
          u <- .new_unit(NA_character_, ut, raw_lines = lines)
          private$.all_units <- c(private$.all_units, list(u))
          next
        }

        u <- private$.parse_unit(ut, lines)
        if (is.null(u)) next

        grp_name <- if (ut %in% names(.UNIT_REGISTRY)) .UNIT_REGISTRY[[ut]]$group else NULL

        if (!is.null(grp_name)) {
          grp <- self[[grp_name]]
          nm  <- u$name
          if (!is.null(nm) && !is.na(nm)) {
            if (nm %in% names(grp)) {
              cli_warn("Duplicate label {.val {nm}} in {grp_name}; keeping first occurrence.")
            } else {
              grp[[nm]] <- u
              self[[grp_name]] <- grp
            }
          }
        } else {
          key <- paste0(u$name %||% "?", " (", ut, ")")
          private$.unsupported[[key]] <- u
        }

        private$.all_units <- c(private$.all_units, list(u))
      }
    },

    .parse_unit = function(ut, lines) {
      n <- private$.label_len
      tryCatch({
        if (ut == "RIVER") {
          sub2 <- toupper(trimws(if (length(lines) >= 2L) lines[2L] else ""))
          if (sub2 == "SECTION")     return(.parse_river_section(lines, n))
          if (sub2 == "FLOOD PLAIN") return(.parse_river_flood_plain(lines, n))
          return(.parse_generic_unit(lines, ut, has_subtype = TRUE, n = n))
        }
        if (ut == "QTBDY")  return(.parse_flow_boundary(lines, "QTBDY", n))
        if (ut == "QHBDY")  return(.parse_qhbdy(lines, n))
        if (ut == "HTBDY")  return(.parse_htbdy(lines, n))
        if (ut %in% c("FSRBDY","FRQSIM","REBDY","FSSR16BDY","GERRBDY","REFHBDY","FEHBDY","SCSBDY"))
          return(.parse_flow_boundary(lines, ut, n))
        if (ut == "BRIDGE") {
          sub2 <- toupper(trimws(if (length(lines) >= 2L) lines[2L] else ""))
          if (sub2 == "USBPR1978") return(.parse_bridge_usbpr1978(lines, n))
          return(.parse_generic_unit(lines, ut, has_subtype = TRUE, n = n))
        }
        if (ut == "JUNCTION") return(.parse_junction(lines, n))

        has_sub <- if (ut %in% names(.UNIT_REGISTRY)) .UNIT_REGISTRY[[ut]]$has_subtype else FALSE
        .parse_generic_unit(lines, ut, has_subtype = has_sub, n = n)

      }, error = function(e) {
        cli_warn("Failed to parse {ut} block: {conditionMessage(e)}")
        NULL
      })
    },

    # ---- GXY locations ---------------------------------------------
    .get_unit_locations = function() {
      gxy <- Filter(nchar, private$.gxy_data)
      i   <- 1L
      loc <- list()
      while (i + 2L <= length(gxy)) {
        header <- gsub("^\\[|\\]$", "", gxy[i])
        parts  <- strsplit(header, "_", fixed = TRUE)[[1L]]
        if (length(parts) < 3L) break
        x <- suppressWarnings(as.numeric(sub("^.*:", "", gxy[i + 1L])))
        y <- suppressWarnings(as.numeric(sub("^.*:", "", gxy[i + 2L])))
        key    <- paste(parts[1L], parts[3L], sep = "_")
        loc[[key]] <- c(x, y)
        i <- i + 3L
      }
      for (u in private$.all_units) {
        key <- paste(u$unit, u$name %||% "", sep = "_")
        if (key %in% names(loc)) u$.location <- loc[[key]]
      }
    },

    # ---- Navigation helpers ----------------------------------------
    .next_in_dat = function(unit) {
      for (i in seq_along(private$.all_units)) {
        if (identical(private$.all_units[[i]], unit)) {
          if (i < length(private$.all_units)) return(private$.all_units[[i + 1L]])
          return(NULL)
        }
      }
      NULL
    },

    .prev_in_dat = function(unit) {
      for (i in seq_along(private$.all_units)) {
        if (identical(private$.all_units[[i]], unit)) {
          if (i > 1L) return(private$.all_units[[i - 1L]])
          return(NULL)
        }
      }
      NULL
    },

    .name_match = function(unit, name_override = NULL) {
      nm <- name_override %||% unit$name
      matches <- Filter(
        function(u) !identical(u, unit) && identical(u$name, nm),
        private$.all_units
      )
      if (length(matches) == 0L) return(NULL)
      if (length(matches) == 1L) return(matches[[1L]])
      matches
    },

    .ds_label_match = function(unit) {
      matches <- Filter(
        function(u) !is.null(u$ds_label) && !is.na(u$ds_label) &&
                    identical(u$ds_label, unit$name),
        private$.all_units
      )
      if (length(matches) == 0L) return(NULL)
      if (length(matches) == 1L) return(matches[[1L]])
      matches
    },

    # ---- Bridge helper: nearest adjacent RIVER SECTION --------------
    .find_adjacent_section = function(bridge_unit) {
      idx <- Position(function(u) identical(u, bridge_unit), private$.all_units)
      if (is.null(idx)) return(NULL)
      for (delta in c(-1L, 1L, -2L, 2L, -3L, 3L)) {
        i <- idx + delta
        if (i < 1L || i > length(private$.all_units)) next
        u <- private$.all_units[[i]]
        if (u$unit == "RIVER" &&
            !is.null(u$data$cross_section) &&
            nrow(u$data$cross_section) > 0L) return(u)
      }
      NULL
    },

    # ---- Group helpers ---------------------------------------------
    .get_group = function(unit) {
      gn <- private$.group_name(unit)
      if (gn == "_unsupported") return(private$.unsupported)
      self[[gn]]
    },

    .set_group = function(unit, grp) {
      gn <- private$.group_name(unit)
      if (gn == "_unsupported") private$.unsupported <- grp
      else self[[gn]] <- grp
    },

    .group_name = function(unit) {
      ut <- unit$unit
      if (ut %in% names(.UNIT_REGISTRY)) .UNIT_REGISTRY[[ut]]$group else "_unsupported"
    },

    .insert_index = function(add_before, add_after, add_at) {
      if (!is.null(add_at)) {
        idx <- add_at
        if (idx < 0L) idx <- length(private$.all_units) + 1L + idx
        return(max(0L, idx))
      }
      ref <- add_before %||% add_after
      for (i in seq_along(private$.all_units)) {
        if (identical(private$.all_units[[i]], ref))
          return(i - 1L + if (!is.null(add_after)) 1L else 0L)
      }
      stop("Reference unit not found in DAT.", call. = FALSE)
    }
  )
)


# ---------------------------------------------------------------------------
# ggrepel helper — uses geom_text_repel when available, falls back to geom_text
# ---------------------------------------------------------------------------

ggrepel_text_if_installed <- function(nodes) {
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(
      data  = nodes,
      aes(x = chainage, y = 0, label = label),
      angle = 45, size = 3, nudge_y = 0.15,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  } else {
    ggplot2::geom_text(
      data  = nodes,
      aes(x = chainage, y = 0, label = label),
      angle = 45, hjust = 0, vjust = -0.6, size = 3,
      show.legend = FALSE
    )
  }
}


# ---------------------------------------------------------------------------
# Convenience wrappers (functional style)
# ---------------------------------------------------------------------------

#' @export
plot_section <- function(dat, label) {
  stopifnot(inherits(dat, "DAT"))
  dat$plot_section(label)
}

#' @export
plot_boundary <- function(dat, label) {
  stopifnot(inherits(dat, "DAT"))
  dat$plot_boundary(label)
}

#' @export
plot_long_section <- function(dat) {
  stopifnot(inherits(dat, "DAT"))
  dat$plot_long_section()
}

#' @export
plot_bridge <- function(dat, label) {
  stopifnot(inherits(dat, "DAT"))
  dat$plot_bridge(label)
}

#' @export
plot_network <- function(dat) {
  stopifnot(inherits(dat, "DAT"))
  dat$plot_network()
}
