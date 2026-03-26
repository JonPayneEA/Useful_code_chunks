# =============================================================================
# MOGREPS-UK Ensemble Data Download from AWS S3
# Met Office Global and Regional Ensemble Prediction System - UK
# Bucket: met-office-uk-ensemble-model-data (eu-west-2)
# Data format: NetCDF (.nc)
# License: CC BY-SA 4.0 (British Crown copyright, Met Office)
# 
# Uses httr2 for direct S3 REST API calls (no aws.s3 dependency).
# No AWS credentials required — fully anonymous access.
# =============================================================================

# — Install packages if needed —
required_pkgs <- c("httr2", "xml2", "ncdf4", "dplyr", "lubridate", "stringr", "tibble")
missing <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0) install.packages(missing)

library(httr2)
library(xml2)
library(ncdf4)
library(dplyr)
library(lubridate)
library(stringr)
library(tibble)

# — Configuration —
BUCKET   <- "met-office-uk-ensemble-model-data"
REGION   <- "eu-west-2"
BASE_URL <- sprintf("https://%s.s3.%s.amazonaws.com", BUCKET, REGION)
OUT_DIR  <- "mogreps_uk_data"
dir.create(OUT_DIR, showWarnings = FALSE)

# =============================================================================
# 1. List top-level prefixes (folders) in the bucket
# =============================================================================

list_top_level <- function(max_keys = 200) {
  message("Listing top-level prefixes…")
  
  resp <- request(BASE_URL) |>
    req_url_query(
      `list-type` = "2",
      delimiter   = "/",
      `max-keys`  = max_keys
    ) |>
    req_perform()
  
  body <- resp_body_string(resp)
  doc  <- read_xml(body)
  
  # S3 ListBucketResult uses a default namespace — must register it
  
  ns <- xml_ns(doc)
  
  prefixes <- xml_find_all(doc, ".//d1:CommonPrefixes/d1:Prefix", ns) |>
    xml_text()
  
  if (length(prefixes) == 0) {
    message("No common prefixes found. Listing object keys instead.")
    prefixes <- xml_find_all(doc, ".//d1:Contents/d1:Key", ns) |>
      xml_text()
  }
  
  return(prefixes)
}

# =============================================================================
# 2. List files under a given prefix
# Handles pagination via ContinuationToken for large folders.
# =============================================================================

list_files <- function(prefix, max_keys = 1000, max_pages = 5) {
  message(sprintf("Listing files under: %s", prefix))
  
  all_rows <- list()
  continuation <- NULL
  page <- 0
  
  repeat {
    page <- page + 1
    if (page > max_pages) {
      message(sprintf("  Stopped after %d pages. Increase max_pages for more.", max_pages))
      break
    }
    
    q <- list(
      `list-type` = "2",
      prefix      = prefix,
      `max-keys`  = max_keys
    )
    if (!is.null(continuation)) {
      q[["continuation-token"]] <- continuation
    }
    
    resp <- request(BASE_URL) |>
      req_url_query(!!!q) |>
      req_perform()
    
    doc <- read_xml(resp_body_string(resp))
    ns  <- xml_ns(doc)
    
    keys  <- xml_find_all(doc, ".//d1:Contents/d1:Key", ns)   |> xml_text()
    sizes <- xml_find_all(doc, ".//d1:Contents/d1:Size", ns)  |> xml_text()
    dates <- xml_find_all(doc, ".//d1:Contents/d1:LastModified", ns) |> xml_text()
    
    if (length(keys) == 0) break
    
    all_rows[[page]] <- tibble(
      key           = keys,
      size_bytes    = as.numeric(sizes),
      last_modified = dates
    )
    
    # Check for more pages
    is_trunc <- xml_find_first(doc, ".//d1:IsTruncated", ns) |> xml_text()
    if (is_trunc != "true") break
    
    continuation <- xml_find_first(doc, ".//d1:NextContinuationToken", ns) |> xml_text()

  }
  
  bind_rows(all_rows) |>
    filter(size_bytes > 0)
}

# =============================================================================
# 3. Download a single NetCDF file from S3
# =============================================================================

download_nc <- function(s3_key, local_dir = OUT_DIR) {
  local_path <- file.path(local_dir, basename(s3_key))
  
  if (file.exists(local_path)) {
    message(sprintf("  Already exists: %s", local_path))
    return(local_path)
  }
  
  url <- paste0(BASE_URL, "/", s3_key)
  message(sprintf("  Downloading: %s", s3_key))
  
  resp <- request(url) |>
    req_perform()
  
  writeBin(resp_body_raw(resp), local_path)
  message(sprintf("  Saved to: %s (%.1f MB)", local_path, file.size(local_path) / 1e6))
return(local_path)
}

# =============================================================================
# 4. Inspect a downloaded NetCDF file
# =============================================================================

inspect_nc <- function(filepath) {
  nc <- nc_open(filepath)
  on.exit(nc_close(nc))
  
  cat("\n===", basename(filepath), "===\n")
  cat("Dimensions:\n")
  for (d in nc$dim) {
    cat(sprintf("  %s: %d values", d$name, d$len))
    if (d$len > 0 && d$len <= 5) {
      cat(sprintf("  [%s]", paste(round(d$vals, 4), collapse = ", ")))
    } else if (d$len > 5) {
      cat(sprintf("  [%s, …, %s]",
                   paste(round(head(d$vals, 3), 4), collapse = ", "),
                   round(tail(d$vals, 1), 4)))
    }
    cat("\n")
  }
  
  cat("\nVariables:\n")
  for (v in nc$var) {
    dims_str <- paste(sapply(v$dim, `[[`, "name"), collapse = " x ")
    cat(sprintf("  %s  (%s)\n", v$name, dims_str))
ln <- ncatt_get(nc, v$name, "long_name")
if (ln$hasatt) cat(sprintf("    long_name: %s\n", ln$value))
un <- ncatt_get(nc, v$name, "units")
if (un$hasatt) cat(sprintf("    units: %s\n", un$value))
  }
  
  invisible(nc)
}

# =============================================================================
# 5. Extract a variable into a tidy data frame
# =============================================================================

nc_to_df <- function(filepath, varname = NULL) {
  nc <- nc_open(filepath)
  on.exit(nc_close(nc))
  
  # Auto-pick first non-coordinate variable
  
  if (is.null(varname)) {
    coord_names <- names(nc$dim)
    data_vars   <- setdiff(names(nc$var), coord_names)
    if (length(data_vars) == 0) stop("No data variables found.")
    varname <- data_vars[1]
    message(sprintf("  Auto-selected variable: %s", varname))
  }
  
  vals <- ncvar_get(nc, varname)
  
  # Build coordinate vectors per dimension
  
  dim_info <- nc$var[[varname]]$dim
  dim_vals <- lapply(dim_info, function(d) {
    v <- d$vals
    # Convert CF-convention time
    if (grepl("time", d$name, ignore.case = TRUE) && d$units != "") {
      parts <- str_match(d$units, "(\\w+)\\s+since\\s+(.+)")
      if (!is.na(parts[1, 1])) {
        origin <- as.POSIXct(parts[1, 3], tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
        if (is.na(origin)) origin <- as.POSIXct(parts[1, 3], tz = "UTC")
        mult <- switch(parts[1, 2],
                       "seconds" = 1, "minutes" = 60, "hours" = 3600, "days" = 86400, 1
        )
        v <- origin + v * mult
      }
    }
    v
  })
  names(dim_vals) <- sapply(dim_info, `[[`, "name")
  
  grid <- expand.grid(rev(dim_vals), KEEP.OUT.ATTRS = FALSE)
  grid <- grid[, rev(names(grid)), drop = FALSE]
  grid[[varname]] <- as.vector(vals)
  
  as_tibble(grid)
}

# =============================================================================
# EXAMPLE USAGE — run interactively, step by step
# =============================================================================

if (interactive()) {
  
  # Step 1: See the folder structure
  top <- list_top_level()
  print(top)
  
  # Step 2: Pick a prefix and list its files
  files <- list_files(prefix = top[1])
  print(files)
  
#   We need
#   These are the paramemeters we've requested from MO 
#   Rainfall accumulation – 1 hour (0-1-65) 
#   Snowfall accumulation 1-hour (0-1-53) 
#   Hail fall accumulation 1-hour (0-1-73) 
#   Temperature at 1.5m (0-0-0)
 
  wanted <- c("rainfall_accumulation-PT01H.nc",
              "snowfall_accumulation-PT01H.nc",
              "hail_fall_accumulation-PT01H.nc", 
              "temperature_at_screen_level_min-PT01H.nc") # I think this is right
  
  # Step 3: Download a small file first
  files <- files |> arrange(size_bytes)
  files$type <- sub("^([^-]*-){2}", "", basename(files$key))
  setDT(files)
  files <- files[type %in% wanted ]
  local_file <- download_nc(files$key[117])
  
  # Step 4: Inspect dimensions and variables
  inspect_nc(local_file)
  
  # Step 5: Load a variable into a tibble
  df <- nc_to_df(local_file)
  
  # head(df)
  
}

