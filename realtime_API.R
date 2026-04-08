# ============================================================ #
# Tool:         ea_flood_monitor
# Description:  Backfill 30-day hydrometric data and archive
#               real-time flood warnings/alerts from the EA
#               Real-Time flood-monitoring API.
# Flode Module: reach.io (candidate)
# Author:       [Your name, email]
# Created:      2026-04-08
# Modified:     2026-04-08 - initial
# Tier:         2
# Inputs:       EA flood-monitoring API (open, no key required)
# Outputs:      Parquet archives under data/hydrometric/ and
#               data/flood_warnings/ (daily rotation)
# Dependencies: httr2, data.table, arrow, logger
#               All are non-tidyverse; fastverse-aligned.
# ============================================================ #

suppressPackageStartupMessages({
  library(httr2)       # modern HTTP client
  library(data.table)  # fastverse core
  library(arrow)       # parquet I/O
  library(logger)      # structured logging
  library(here)        # portable paths
})

# ── Configuration ──────────────────────────────────────────── #

EA_API_ROOT  <- "https://environment.data.gov.uk/flood-monitoring"
USER_AGENT   <- "reach.io/0.1 (EA flood monitoring archiver)"

# Output directories (created if absent)
DIR_HYDRO    <- here::here("data", "hydrometric")
DIR_WARNINGS <- here::here("data", "flood_warnings")

dir.create(DIR_HYDRO,    showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_WARNINGS, showWarnings = FALSE, recursive = TRUE)

# Logger setup
dir.create(here::here("logs"), showWarnings = FALSE, recursive = TRUE)
log_appender(appender_file(here::here("logs", "ea_monitor.log"),
                           append = TRUE))
log_threshold(INFO)

# ── Low-level HTTP helper ───────────────────────────────────── #

#' Make a GET request against the EA API and return parsed JSON items.
#'
#' @param path Character. Path appended to EA_API_ROOT.
#' @param query Named list of query parameters.
#' @param retries Integer. Number of retry attempts on transient failure.
#' @return List (parsed JSON `items` element) or NULL on failure.
fetch_ea <- function(path, query = list(), retries = 5L) {
  url <- paste0(EA_API_ROOT, path)
  
  # Backoff with jitter: base 2^attempt seconds + uniform noise up to 1s.
  # Covers 503 Service Unavailable and 429 Too Many Requests explicitly.
  jitter_backoff <- function(attempt) (2 ^ attempt) + stats::runif(1, 0, 1)
  is_transient_ea <- function(resp) {
    httr2::resp_status(resp) %in% c(429L, 500L, 503L, 504L)
  }
  
  req <- request(url) |>
    req_user_agent(USER_AGENT) |>
    req_url_query(!!!query) |>
    req_retry(
      max_tries    = retries,
      is_transient = is_transient_ea,
      backoff      = jitter_backoff
    ) |>
    req_timeout(90)
  
  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      log_error("HTTP error fetching {url}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(resp)) return(NULL)
  
  parsed <- tryCatch(
    resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      log_error("JSON parse error for {url}: {conditionMessage(e)}")
      NULL
    }
  )
  parsed[["items"]]
}

# ── Station catalogue ───────────────────────────────────────── #

#' Download the full station catalogue and return as data.table.
#'
#' @param parameter Character. Filter by parameter e.g. "level", "flow".
#' @return data.table of stations.
fetch_stations <- function(parameter = NULL) {
  log_info("Fetching station catalogue (parameter={parameter %||% 'all'})")
  q <- list(`_limit` = 10000L)
  if (!is.null(parameter)) q[["parameter"]] <- parameter
  
  items <- fetch_ea("/id/stations", query = q)
  if (is.null(items)) return(data.table())
  
  rbindlist(
    lapply(items, function(x) {
      data.table(
        station_ref  = x[["stationReference"]] %||% NA_character_,
        label        = x[["label"]]            %||% NA_character_,
        river_name   = x[["riverName"]]        %||% NA_character_,
        catchment    = x[["catchmentName"]]    %||% NA_character_,
        lat          = x[["lat"]]              %||% NA_real_,
        lon          = x[["long"]]             %||% NA_real_,
        town         = x[["town"]]             %||% NA_character_,
        status       = x[["status"]]           %||% NA_character_,
        api_uri      = x[["@id"]]              %||% NA_character_
      )
    }),
    fill = TRUE
  )
}

# ── Hydrometric backfill ─────────────────────────────────────── #
#
# Strategy: the API recommends /data/readings?date=d which returns ALL
# stations for that date in a single call. 30 days = 30 requests total,
# not 30 * N_stations. This is the correct way to avoid 503s.

#' Parse a list of raw reading items into a data.table.
#' @param items List. Raw JSON items from the API.
#' @return data.table
parse_readings <- function(items) {
  rbindlist(
    lapply(items, function(x) {
      # With _view=full the API may embed measure as a nested object
      # rather than a plain URI string; extract @id in that case.
      m           <- x[["measure"]]
      measure_uri <- if (is.list(m)) {
        as.character(m[["@id"]] %||% NA_character_)
      } else {
        as.character(m %||% NA_character_)
      }
      data.table(
        station_ref  = sub(".*/stations/([^/]+)/.*", "\\1", measure_uri),
        measure_id   = basename(measure_uri),
        datetime_utc = x[["dateTime"]]  %||% NA_character_,
        value        = x[["value"]]     %||% NA_real_,
        unit_name    = x[["unitName"]]  %||% NA_character_,
        parameter    = x[["parameter"]] %||% NA_character_,
        qualifier    = x[["qualifier"]] %||% NA_character_
      )
    }),
    fill = TRUE
  )[, datetime_utc := as.POSIXct(datetime_utc,
                                 format = "%Y-%m-%dT%H:%M:%OSZ",
                                 tz = "UTC")]
}

#' Backfill all stations for one date using the bulk readings endpoint.
#'
#' One HTTP call returns every station for that day. The result is written
#' to a single date-partitioned Parquet file.
#'
#' @param date_str Character. ISO date "YYYY-MM-DD".
#' @param parameter Character. Optional parameter filter e.g. "level".
#' @param throttle_s Numeric. Seconds to sleep after the request.
#' @return Invisible NULL.
backfill_date <- function(date_str, parameter = NULL, throttle_s = 1.0) {
  out_file <- file.path(DIR_HYDRO, sprintf("date=%s.parquet", date_str))
  
  if (file.exists(out_file)) {
    log_info("  {date_str}: already cached -- skipping")
    return(invisible(NULL))
  }
  
  q <- list(date = date_str, `_limit` = 100000L, `_view` = "full")
  if (!is.null(parameter)) q[["parameter"]] <- parameter
  
  items <- fetch_ea("/data/readings", query = q)
  
  Sys.sleep(throttle_s + stats::runif(1, 0, 0.5))  # polite throttle + jitter
  
  if (is.null(items) || length(items) == 0L) {
    log_warn("  {date_str}: no readings returned")
    return(invisible(NULL))
  }
  
  dt <- parse_readings(items)
  dir.create(DIR_HYDRO, showWarnings = FALSE, recursive = TRUE)
  write_parquet(dt, out_file)
  log_info("  {date_str}: {nrow(dt)} rows, {uniqueN(dt$station_ref)} stations -> {out_file}")
  invisible(NULL)
}

#' Backfill the past N days for all stations (30 API calls total).
#'
#' @param days      Integer. Days to backfill (max 30 available from API).
#' @param parameter Character. Optional parameter filter.
#' @param throttle_s Numeric. Base sleep between requests in seconds.
#' @return Invisible NULL.
backfill_readings <- function(days = 28L, parameter = NULL, throttle_s = 1.5) {
  end_date   <- Sys.Date() - 1L
  start_date <- end_date - (days - 1L)
  dates      <- as.character(seq(start_date, end_date, by = "day"))
  
  log_info("Starting bulk backfill: {start_date} to {end_date} ({length(dates)} requests)")
  
  for (d in dates) {
    backfill_date(d, parameter = parameter, throttle_s = throttle_s)
  }
  
  log_info("Backfill complete")
  invisible(NULL)
}

# ── Incremental hydrometric update ──────────────────────────── #

#' Fetch readings for one station since a given datetime.
#'
#' Uses the `since` filter which is the API-recommended way to
#' maintain a continuous time series after initial backfill.
#'
#' @param station_ref  Character. Station reference.
#' @param since_dt     POSIXct. Fetch all readings after this timestamp.
#' @return data.table of new readings (0-row if none).
fetch_since <- function(station_ref, since_dt) {
  since_str <- format(since_dt, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  
  items <- fetch_ea(
    path  = sprintf("/id/stations/%s/readings", station_ref),
    query = list(since = since_str, `_sorted` = "", `_limit` = 10000L)
  )
  if (is.null(items) || length(items) == 0L) return(data.table())
  
  rbindlist(
    lapply(items, function(x) {
      data.table(
        station_ref  = station_ref,
        measure_id   = basename(x[["measure"]] %||% NA_character_),
        datetime_utc = x[["dateTime"]]         %||% NA_character_,
        value        = x[["value"]]            %||% NA_real_,
        unit_name    = x[["unitName"]]         %||% NA_character_,
        parameter    = x[["parameter"]]        %||% NA_character_,
        qualifier    = x[["qualifier"]]        %||% NA_character_
      )
    }),
    fill = TRUE
  )[, datetime_utc := as.POSIXct(datetime_utc,
                                 format = "%Y-%m-%dT%H:%M:%OSZ",
                                 tz = "UTC")]
}

# ── Incremental hydrometric update ──────────────────────────── #

#' Fetch all readings since a given datetime (incremental update).
#'
#' Uses the `since` filter on the bulk endpoint -- one call regardless
#' of how many stations have updated. For 15-min cadence pass a
#' since_dt 20 minutes in the past to tolerate late arrivals.
#'
#' @param since_dt   POSIXct UTC. Fetch all readings after this timestamp.
#' @param parameter  Character. Optional parameter filter.
#' @return data.table of new readings (0-row if none).
fetch_readings_since <- function(since_dt, parameter = NULL) {
  since_str <- format(since_dt, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  log_info("Incremental fetch since {since_str}")
  
  q <- list(since = since_str, `_limit` = 100000L, `_view` = "full")
  if (!is.null(parameter)) q[["parameter"]] <- parameter
  
  items <- fetch_ea("/data/readings", query = q)
  if (is.null(items) || length(items) == 0L) {
    log_info("  No new readings")
    return(data.table())
  }
  dt <- parse_readings(items)
  log_info("  {nrow(dt)} new readings for {uniqueN(dt$station_ref)} stations")
  dt
}

#' Append incremental readings to the appropriate date-partitioned Parquet files.
#'
#' Splits the incoming data.table by date and upserts into existing Parquet
#' partitions (or creates new ones). Duplicate rows (same station + datetime)
#' are dropped so re-runs are safe.
#'
#' @param dt data.table. Output of fetch_readings_since().
#' @return Invisible NULL.
append_readings <- function(dt) {
  if (nrow(dt) == 0L) return(invisible(NULL))
  
  dt[, date_str := format(datetime_utc, "%Y-%m-%d", tz = "UTC")]
  key_cols <- c("station_ref", "measure_id", "datetime_utc")
  
  for (d in unique(dt$date_str)) {
    out_file <- file.path(DIR_HYDRO, sprintf("date=%s.parquet", d))
    new_rows  <- dt[date_str == d, !"date_str"]
    
    if (file.exists(out_file)) {
      existing <- as.data.table(read_parquet(out_file))
      combined <- unique(rbindlist(list(existing, new_rows), fill = TRUE),
                         by = key_cols)
    } else {
      combined <- unique(new_rows, by = key_cols)
    }
    
    write_parquet(combined, out_file)
    log_info("  {d}: {nrow(combined)} rows after upsert")
  }
  invisible(NULL)
}

#' High-level update function -- call this on a cron schedule.
#'
#' Reads the latest timestamp from the most recent Parquet partition,
#' fetches everything since then, and appends.
#'
#' @param lookback_mins Integer. How far back to look if no cache exists.
#' @param parameter     Character. Optional parameter filter.
#' @return Invisible NULL.
update_readings <- function(lookback_mins = 30L, parameter = NULL) {
  # Determine since_dt from newest cached file
  parquet_files <- list.files(DIR_HYDRO, pattern = "\\.parquet$",
                              full.names = TRUE, recursive = FALSE)
  
  if (length(parquet_files) > 0L) {
    latest_file <- parquet_files[which.max(file.mtime(parquet_files))]
    cached      <- as.data.table(read_parquet(latest_file))
    since_dt    <- max(cached$datetime_utc, na.rm = TRUE)
    # Subtract 5 min to catch any late-arriving records
    since_dt    <- since_dt - 5L * 60L
  } else {
    since_dt <- Sys.time() - as.difftime(lookback_mins, units = "mins")
  }
  
  dt <- fetch_readings_since(since_dt, parameter = parameter)
  append_readings(dt)
  invisible(NULL)
}

# ── Flood Warning / Alert archive ───────────────────────────── #

#' Fetch all current flood warnings and alerts.
#'
#' @param min_severity Integer. 1 = Severe only, 3 = all active, 4 = include expired.
#' @return data.table of current warnings.
fetch_warnings <- function(min_severity = 3L) {
  items <- fetch_ea("/id/floods", query = list(`min-severity` = min_severity))
  if (is.null(items) || length(items) == 0L) return(data.table())
  
  rbindlist(
    lapply(items, function(x) {
      fa <- x[["floodArea"]] %||% list()
      data.table(
        warning_id           = basename(x[["@id"]] %||% NA_character_),
        flood_area_id        = x[["floodAreaID"]]       %||% NA_character_,
        severity_level       = x[["severityLevel"]]     %||% NA_integer_,
        severity             = x[["severity"]]          %||% NA_character_,
        description          = x[["description"]]       %||% NA_character_,
        message              = x[["message"]]           %||% NA_character_,
        ea_area              = x[["eaAreaName"]]        %||% NA_character_,
        is_tidal             = x[["isTidal"]]           %||% NA,
        time_raised          = x[["timeRaised"]]        %||% NA_character_,
        time_severity_changed= x[["timeSeverityChanged"]] %||% NA_character_,
        time_message_changed = x[["timeMessageChanged"]] %||% NA_character_,
        county               = fa[["county"]]           %||% NA_character_,
        river_or_sea         = fa[["riverOrSea"]]       %||% NA_character_,
        fetched_utc          = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      )
    }),
    fill = TRUE
  )[, c("time_raised", "time_severity_changed", "time_message_changed") :=
      lapply(.SD, function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")),
    .SDcols = c("time_raised", "time_severity_changed", "time_message_changed")]
}

#' Archive a snapshot of current warnings to a timestamped Parquet file.
#'
#' Each poll writes a new file; use the archive to reconstruct warning
#' history including changes to message and severity over time.
#'
#' @param min_severity Integer. Passed to fetch_warnings().
#' @return Invisible NULL.
archive_warnings <- function(min_severity = 3L) {
  dt <- fetch_warnings(min_severity = min_severity)
  
  if (nrow(dt) == 0L) {
    log_info("Warnings archive: no active warnings at {Sys.time()}")
    return(invisible(NULL))
  }
  
  # Partition by date; filename encodes the snapshot time for ordering
  snap_ts   <- format(Sys.time(), "%Y-%m-%dT%H-%M-%SZ", tz = "UTC")
  snap_date <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
  out_dir   <- file.path(DIR_WARNINGS, sprintf("date=%s", snap_date))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  out_file <- file.path(out_dir, sprintf("warnings_%s.parquet", snap_ts))
  write_parquet(dt, out_file)
  log_info("Warnings: {nrow(dt)} active (severity <= {min_severity}) -> {out_file}")
  invisible(NULL)
}

#' Load the full warnings archive into a single data.table.
#'
#' @param date_from Character "YYYY-MM-DD". Optional start filter.
#' @return data.table of all archived warning snapshots.
load_warnings_archive <- function(date_from = NULL) {
  dirs <- list.dirs(DIR_WARNINGS, recursive = FALSE, full.names = TRUE)
  
  if (!is.null(date_from)) {
    dirs <- dirs[basename(dirs) >= sprintf("date=%s", date_from)]
  }
  
  files <- unlist(lapply(dirs, list.files,
                         pattern = "\\.parquet$", full.names = TRUE))
  if (length(files) == 0L) return(data.table())
  
  rbindlist(lapply(files, function(f) as.data.table(read_parquet(f))),
            fill = TRUE)
}

# ── Convenience wrappers ─────────────────────────────────────── #

#' Run a complete initial setup: backfill 30 days + archive current warnings.
setup_archive <- function() {
  log_info("=== EA flood monitor: initial setup ===")
  backfill_readings(days = 28L)
  archive_warnings()
  log_info("=== Setup complete ===")
}

#' Scheduled update -- suitable for cron at 15-min, hourly, or daily cadence.
#'
#' @param warn_severity Integer. Min severity for warning archive (default 3).
scheduled_update <- function(warn_severity = 3L) {
  log_info("=== Scheduled update: {Sys.time()} ===")
  update_readings()
  archive_warnings(min_severity = warn_severity)
}

# ── Null-coalescing helper (base R has no %||% before R 4.4) ── #
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Usage ───────────────────────────────────────────────────── #
source("ea_flood_monitor.R")

# One-time setup
setup_archive()                        # 28 requests for hydro + 1 for warnings

# Cron: every 15 min
scheduled_update()

# Or call independently
update_readings()                      # hydro only
archive_warnings(min_severity = 2L)   # FW + SFW only

# Read back
archive <- load_warnings_archive(date_from = "2026-04-01")


  
