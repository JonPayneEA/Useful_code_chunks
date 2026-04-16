# Useful Code Chunks

This repository contains a collection of useful code snippets and functions across various programming languages. The goal is to provide quick reference access to commonly used programming tasks and algorithms.

## Comprehensive Function List

### FMP_Node_Geometry.R
Parsers for Flood Modeller Pro (FMP) spatial files. Returns `sf` objects in EPSG:27700 (British National Grid).

- **parse_gxy():** Parses `.gxy` files to LINESTRING sf (spatial features) per reach
- **parse_dat():** Parses `.dat` files to POINT sf per node, or optionally LINESTRING per reach
- **model_geometry():** Wrapper that tries GXY first, falls back to DAT, and attaches `model_id`
- **batch_model_geometry():** Applies `model_geometry()` across a data frame of multiple models

**Dependencies:** `sf`, `data.table`, `cli`

---

### CrossSection_Rating_Overlay.R
Produces a single combined plot with a river cross-section and a rating curve overlaid on the same axes using a dual x-axis (secondary axis) scaling trick.

- Cross-section bed rendered as a filled ribbon with water-fill polygons at three flow stages (Low flow, Bankfull, Flood)
- Rating curve (Manning-style power law: `Q = a·(H − H₀)^b`) scaled onto the channel distance axis and overlaid as an orange line, colour-coded above/below bankfull
- Gauged observation points plotted on the rating curve
- Secondary x-axis labels discharge in m³/s; primary x-axis shows distance across channel in metres
- **make_water_poly(wl, xs):** Builds a closed polygon representing the water surface at a given water level `wl` from cross-section coordinates `xs`; clips to wetted nodes and caps at the water surface elevation
- **q_to_dist(q):** Linearly scales a discharge value onto the channel distance axis for the dual-axis overlay
- **dist_to_q(d):** Inverse of `q_to_dist()`; used by `sec_axis` to label the secondary x-axis in discharge units

**Dependencies:** `ggplot2`, `dplyr`, `scales`

---

### CrossSection_Rating_Dual_Plot.R
Produces two separate, vertically stacked plots combined with `patchwork`:

- **Plot A – Cross-Section:** Channel bed with water-fill polygons and dashed water surface lines at three flow stages, with inline stage labels
- **Plot B – Rating Curve:** Stage–discharge curve with above-bankfull zone shading, colour change at bankfull, and simulated gauged observation points
- **make_water_poly(wl, xs):** Builds a closed polygon representing the water surface at a given water level `wl` from cross-section coordinates `xs`; used internally to fill the channel at each flow stage

**Dependencies:** `ggplot2`, `patchwork`, `dplyr`

---

### RatingCurve_Gap_Check.R
Utilities for working with multi-limb rating curves in the standard form **Q = C(h − a)^b**, including expanding a rating equation table into stage–discharge data, detecting discharge discontinuities ("gaps") at limb junctions, resolving those gaps, and producing diagnostic plots. When limbs are fitted separately their endpoints rarely align exactly, producing visual gaps and hydraulic inconsistencies.

- **expand_rating_table():** Converts a rating equation table (one row per limb with columns Lower level, Upper level, C, A, B, and an optional Doubtful flag) into a stage–discharge data frame ready for the functions below. Evaluates `Q = C(h − A)^B` at a configurable stage increment; both endpoints of each limb are always included so adjacent limbs share the breakpoint stage, which is where gaps are measured. Accepts a `max_stage` parameter to cap open-ended sentinel upper levels (e.g. `999`).
- **detect_rc_gaps():** Scans every limb junction and reports the absolute and relative discharge gap; flags junctions that exceed configurable tolerances (`tol_abs`, `tol_rel`). Accepts an explicit limb-ID column or auto-detects limbs from monotonicity breaks in discharge. Works for any number of limbs.
- **resolve_rc_gaps():** Closes flagged gaps using one of three strategies:
  - `"interpolate"` *(default)* — averages the two limbs' bridge estimates at the breakpoint stage so both limbs meet at an agreed discharge without distorting either curve's shape
  - `"snap_to_lower"` — shifts the upper limb's starting discharge to match the lower limb's end (use when the lower/gauged limb is trusted)
  - `"snap_to_upper"` — shifts the lower limb's ending discharge to match the upper limb's start (use when the upper limb anchor, e.g. a flood-frequency estimate flagged as doubtful, is trusted)
- **plot_rc_gaps():** Diagnostic ggplot overlaying the original (dashed) and corrected (solid) curves, coloured by limb. Limbs flagged as doubtful are hidden from both curves. Resolved junctions are marked with a filled dot; flagged junctions are annotated at the right margin with stage and ΔQ, staggered vertically to prevent overlap. Uses `geom_path` so lines are always drawn in stage order — necessary because gap resolution can make discharge temporarily non-monotonic at a corrected limb's first point. Supports any number of limbs.

**Dependencies:** `ggplot2` (base R otherwise)

---

### gr4j.R
4-parameter lumped conceptual rainfall-runoff model (GR4J-style). Simulates daily streamflow from precipitation and potential evapotranspiration using a production store, groundwater exchange, routing store, and unit hydrograph convolution. Includes synthetic UK-ish climate data generation, parameter sensitivity analysis, and diagnostic plots (hydrograph + flow duration curve).

- **gr4j_run(P, PET, x1, x2, x3, x4):** Runs the model for a precipitation and PET time series. Returns a numeric vector of simulated daily flows (mm/day).
  - `x1` — production store capacity (mm), typical 100–1500
  - `x2` — groundwater exchange coefficient (mm/day), negative = net loss, typical −5 to 5
  - `x3` — routing store capacity (mm), typical 10–500
  - `x4` — unit hydrograph time base (days), typical 0.5–4

**Dependencies:** `data.table`

---

### MOGREPS_AWS_Download.R
Downloads MOGREPS-UK ensemble weather forecast data (NetCDF) from the Met Office public AWS S3 bucket (`met-office-uk-ensemble-model-data`, eu-west-2). No AWS credentials required — fully anonymous S3 REST API access via `httr2`. Handles S3 paginated listing, skip-if-exists downloads, NetCDF inspection, and tidy extraction of any variable into a data frame. Licence: CC BY-SA 4.0.

- **list_top_level():** Lists top-level prefixes (folders) in the S3 bucket; returns a character vector of prefix strings.
- **list_files(prefix, max_keys, max_pages):** Lists all `.nc` object keys under a given prefix, paginating via `ContinuationToken`; returns a tibble of `key`, `size_bytes`, `last_modified`.
- **download_nc(s3_key, local_dir):** Downloads a single NetCDF file to `local_dir`; skips if already present. Returns the local file path.
- **inspect_nc(filepath):** Prints dimension names/sizes/ranges and variable metadata (long_name, units) for a downloaded NetCDF file.
- **nc_to_df(filepath, varname):** Reads a NetCDF variable into a tidy tibble with one column per dimension plus the variable values. CF-convention time axes are automatically decoded to `POSIXct`. Auto-selects the first non-coordinate variable if `varname` is `NULL`.

**Dependencies:** `httr2`, `xml2`, `ncdf4`, `dplyr`, `lubridate`, `stringr`, `tibble`

---

### River_Network_Graphs.R
Builds a large synthetic ~75-reach dendritic catchment (Nene-scale), simplifies it into a contracted graph retaining only confluences and gauge locations, and produces two publication-quality plots with `patchwork`: a spatial map and a schematic flow diagram. The simplification algorithm scales to any network size without modification.

- **make_reach(coords, id):** Creates a single `sf` LINESTRING feature from a 2-column coordinate matrix and a reach ID string.
- **make_chain(waypoints, id_prefix, start_id):** Builds a sequential chain of reaches from an ordered matrix of waypoints; returns a list of `sf` objects with auto-incremented IDs.
- **get_endpoints(geom):** Extracts the start and end point coordinates of an `sf` LINESTRING geometry as a 2-row matrix.
- **make_node_id(xy, digits):** Rounds coordinates to `digits` decimal places and concatenates them into a string node identifier (used as graph vertex names).
- **assign_x(node, x_left, x_right):** Recursive function that assigns horizontal positions for the schematic layout by splitting the `[x_left, x_right]` interval across tributaries in upstream order.

**Dependencies:** `sf`, `igraph`, `ggplot2`, `patchwork`

---

### extract_network_svg.R
Parses a flood-network SVG diagram (e.g. exported from a graphviz/dot layout) to extract a directed node-link table with display labels. Matches edge endpoints to the nearest node port using a configurable spatial tolerance. Designed for SVGs where each node is a `<g class="node …">` element with a `<text>/<tspan>` label, and edges are `<path>` elements inside `<g id="edges">`.

- **extract_svg_links(svg_file, tol):** Reads `svg_file`, parses all node ports and edge paths, snaps edge endpoints to the nearest port within `tol` pixels, and returns a named list:
  - `$ports` — `data.table` of node IDs, display names, class, and port centre coordinates
  - `$edges` — `data.table` of directed links with `from_name`, `to_name`, stroke style, dashed flag, and a `suspect` flag for any endpoint that did not snap within `tol`

**Dependencies:** `xml2`, `data.table`

---

### realtime_API.R
Archiver for the Environment Agency (EA) Real-Time flood monitoring API (open, no authentication key required). Backfills up to 28 days of hydrometric readings, incrementally updates a local Parquet store, and archives flood warnings. Designed for scheduled execution (e.g. hourly cron).

- **fetch_ea(path, query, retries):** Low-level wrapper around the EA API base URL with retry/back-off; returns parsed JSON as a list.
- **fetch_stations(parameter):** Returns a `data.table` of all EA monitoring stations, optionally filtered by parameter type (e.g. `"flow"`, `"level"`).
- **parse_readings(items):** Converts a list of EA API reading items to a tidy `data.table` with `dateTime`, `value`, and `station` columns.
- **backfill_date(date_str, parameter, throttle_s):** Downloads all readings for a single calendar date across all stations; throttles requests to avoid rate-limiting.
- **backfill_readings(days, parameter, throttle_s):** Iterates `backfill_date()` over the most recent `days` days to build an initial archive.
- **fetch_since(station_ref, since_dt):** Fetches readings for a single station since a given `POSIXct` timestamp.
- **fetch_readings_since(since_dt, parameter):** Fetches readings for all stations since `since_dt` in a single paginated call.
- **append_readings(dt):** Appends new rows to the local Parquet readings store, deduplicating on `dateTime` + `station`.
- **update_readings(lookback_mins, parameter):** Incremental update — fetches readings from the last `lookback_mins` minutes and appends them.
- **fetch_warnings(min_severity):** Returns current EA flood warnings at or above `min_severity` (1 = Severe, 2 = Warning, 3 = Alert, 4 = Removed).
- **archive_warnings(min_severity):** Appends current warnings to the local warnings Parquet store.
- **load_warnings_archive(date_from):** Reads the warnings Parquet store, optionally filtered to records from `date_from` onwards.
- **setup_archive():** Creates the local directory structure and empty Parquet files required by the archiver.
- **scheduled_update(warn_severity):** Top-level entry point — runs `update_readings()` and `archive_warnings()` together; intended to be called by a scheduler.

**Dependencies:** `httr2`, `data.table`, `arrow`, `logger`, `here`

---

Feel free to contribute and add more snippets or improve the existing ones!
