library(data.table)

# =============================================================================
# water_units.R
# =============================================================================
# Water volume conversion and contextualisation utilities.
# UK-focused throughout: apothecary values are UK; reservoir capacities are
# from published WRMPs and EA/Ofwat records.
#
# Functions:
#   format_equivalent()         format a numeric equivalent for display
#   make_water_table()          convert a volume to all reference units
#   nearest_units()             find the closest reference units by magnitude
#   rainfall_volume()           compute gross rainfall volume over a catchment
#   nearest_units_from_result() nearest_units() wrapper for rainfall_volume output
#   reservoir_fill()            express a volume as a fraction of UK reservoirs
# =============================================================================


# =============================================================================
# Reference data
# =============================================================================

# -----------------------------------------------------------------------------
# input_unit_factors
#
# Conversion factors from named input units to cubic metres.
# Add rows here to support additional input units; no function changes needed.
# -----------------------------------------------------------------------------
input_unit_factors <- c(
  m3          = 1,        # base unit
  litre       = 1e-3,     # 1/1000 m³
  megalitre   = 1e3,      # 10^6 litres; common in reservoir reporting
  gigalitre   = 1e6,      # 10^9 litres; common in catchment-scale hydrology
  km3         = 1e9,      # cubic kilometres; used for ice sheets, large lakes
  acre_foot   = 1233.48,  # US irrigation and water rights unit
  fluid_oz_uk = 2.841e-5, # UK fluid ounce
  ml          = 1e-6      # millilitre
)


# -----------------------------------------------------------------------------
# unit_reference
#
# Master reference table of named water volumes.
# UK apothecary values used throughout.
# flux = TRUE flags units derived from flow rates rather than static volumes;
# these are separated into $flux by make_water_table().
#
# Note: Rutland Water capacity listed here as 124 GL (older figure).
# WRMP19 gives 130 GL as licensed capacity. Resolve against current WRMP
# before using in published output.
# -----------------------------------------------------------------------------
unit_reference <- data.table(

  display_name = c(
    # SI
    "Cubic metre", "Litre", "Megalitre", "Gigalitre",
    # Domestic / journalistic
    "Bath", "Olympic swimming pool",
    # Agricultural / legal
    "Acre-foot",
    # Apothecary (UK values; retained for amusement and dimensional contrast)
    "Minim (UK)", "Fluid drachm (UK)", "Fluid ounce (UK)",
    "Jigger (UK)", "Hogshead (UK)",
    # Regional informal
    "Sydharb", "Lake Windermere", "Rutland Water",
    # Planetary / oceanographic
    "Dead Sea", "Sverdrup-second", "Global river discharge (annual)"
  ),

  category = c(
    "SI", "SI", "SI", "SI",
    "Domestic", "Domestic",
    "Agricultural",
    "Apothecary", "Apothecary", "Apothecary", "Apothecary", "Apothecary",
    "Regional informal", "Regional informal", "Regional informal",
    "Planetary", "Planetary", "Planetary"
  ),

  cubic_metres = c(
    # SI
    1,        # cubic metre
    1e-3,     # litre
    1e3,      # megalitre
    1e6,      # gigalitre
    # Domestic
    0.15,     # bath (approximate; varies by press release)
    2500,     # olympic swimming pool
    # Agricultural
    1233.48,  # acre-foot (US; no UK equivalent in common use)
    # Apothecary (UK)
    5.919e-8, # minim (UK): 1/60 fluid drachm; approximately one drop
    3.552e-6, # fluid drachm (UK)
    2.841e-5, # fluid ounce (UK)
    4.261e-5, # jigger (UK): 1.5 UK fl oz
    0.2909,   # hogshead (UK): 54 imperial gallons
    # Regional informal
    5e8,      # sydharb: Sydney Harbour, ~500 GL; Australian drought unit
    3.14e8,   # Lake Windermere: 314 million m³; half a Windermere also used
    1.24e8,   # Rutland Water: see note above
    # Planetary
    1.47e11,  # Dead Sea: ~147 km³
    1e6,      # Sverdrup-second: volume flux at 1 Sverdrup for 1 second
    3.7e13    # global river discharge: annual land-to-sea flux
  ),

  flux = c(
    FALSE, FALSE, FALSE, FALSE,        # SI
    FALSE, FALSE,                      # Domestic
    FALSE,                             # Agricultural
    FALSE, FALSE, FALSE, FALSE, FALSE, # Apothecary
    FALSE, FALSE, FALSE,               # Regional informal
    FALSE, TRUE, TRUE                  # Planetary
  ),

  # Explicit plurals for irregular forms. NA defers to the default rule
  # (append 's'), which is correct for most units in the table.
  plural = c(
    # SI
    "Cubic metres", NA, NA, NA,
    # Domestic
    NA, "Olympic swimming pools",
    # Agricultural
    "Acre-feet",                        # irregular
    # Apothecary
    NA, NA, NA, NA, NA,
    # Regional informal
    NA, NA, NA,
    # Planetary
    NA, NA, NA
  )
)


# -----------------------------------------------------------------------------
# uk_reservoirs
#
# UK reservoir capacities from published WRMPs and EA/Ofwat records.
# Total (gross) capacity; usable capacity is typically 85-95% of this.
# -----------------------------------------------------------------------------
uk_reservoirs <- data.table(
  name = c(
    "Kielder Water",      # Northumberland; largest by volume in UK
    "Rutland Water",      # East Midlands; largest by area
    "Haweswater",         # Cumbria; Manchester supply
    "Thirlmere",          # Cumbria; Manchester supply
    "Llyn Brianne",       # Ceredigion; Tywi regulation
    "Grafham Water",      # Cambridgeshire
    "Clywedog",           # Powys; Severn regulation
    "Abberton Reservoir", # Essex
    "Roadford Lake",      # Devon; largest in SW England
    "Bewl Water"          # Kent/East Sussex; largest in SE England
  ),
  capacity_GL = c(
    199.0,  # Kielder
    130.0,  # Rutland Water (WRMP19; see note in unit_reference)
    84.8,   # Haweswater
    41.2,   # Thirlmere
    60.0,   # Llyn Brianne
    59.1,   # Grafham Water
    49.7,   # Clywedog
    42.3,   # Abberton
    34.5,   # Roadford
    31.0    # Bewl Water
  ),
  region = c(
    "North East", "East Midlands", "North West", "North West",
    "Wales", "Anglian", "Wales", "Anglian", "South West", "South East"
  ),
  primary_use = c(
    "Supply/regulation", "Supply", "Supply", "Supply",
    "Regulation", "Supply", "Regulation", "Supply", "Supply", "Supply"
  )
)

# Capacity in m³ for arithmetic consistency with the rest of the file
uk_reservoirs[, capacity_m3 := capacity_GL * 1e6]


# =============================================================================
# Functions
# =============================================================================

# -----------------------------------------------------------------------------
# format_equivalent()
#
# Formats a numeric equivalent for readable display. Switches between:
#   - Scientific notation for values < 0.01   ("6.80e-03 Dead Seas")
#   - Fixed form for values 0.01 to 999       ("12.4 Olympic pools")
#   - Scientific notation above 999           ("1.62e+07 baths")
#
# Arguments:
#   x  numeric vector of equivalents
#
# Returns:
#   character vector of formatted strings
# -----------------------------------------------------------------------------
format_equivalent <- function(x) {

  fmt_single <- function(val) {
    if (is.na(val)) return(NA_character_)
    if (val == 0)   return("0")

    if (abs(val) < 0.01) {
      formatC(val, format = "g", digits = 3)            # e.g. 6.80e-03
    } else if (abs(val) < 1000) {
      # gsub removes the trailing period that formatC "fg" leaves on
      # whole numbers, e.g. "400." becomes "400"; trimws does not catch it
      # because the period is not whitespace
      gsub("\\.$", "", formatC(val, format = "fg", digits = 3,  # e.g. 12.4
              flag = "#"))
    } else {
      formatC(val, format = "e", digits = 2)            # e.g. 1.62e+07
    }
  }

  vapply(x, fmt_single, character(1))
}


# -----------------------------------------------------------------------------
# make_water_table()
#
# Converts a user-supplied volume into equivalent quantities across all
# reference units. Returns a named list with two data.tables:
#   $volumes  static volume comparisons, sorted ascending by unit size
#   $flux     flux-derived units (Sverdrup-second, global river discharge)
#
# Arguments:
#   value  numeric; the volume to convert; must be a single positive number
#   unit   character; input unit, must be one of names(input_unit_factors)
#
# Each data.table contains:
#   display_name   human-readable unit label
#   category       SI / Domestic / Agricultural / Apothecary /
#                  Regional informal / Planetary
#   cubic_metres   size of one reference unit in m³
#   equivalent     how many of this unit the input volume equals (numeric)
#   equivalent_fmt formatted string for display
#   input          original input recorded for traceability
# -----------------------------------------------------------------------------
make_water_table <- function(value, unit) {

  # match.arg() gives an informative error if unit is not recognised;
  # partial matching is intentional (e.g. "gl" matches "gigalitre")
  unit <- match.arg(unit, names(input_unit_factors))

  if (!is.numeric(value) || length(value) != 1 || value <= 0)
    stop("`value` must be a single positive number.")

  # Convert to m³ first; all reference comparisons are in m³
  value_m3 <- value * input_unit_factors[[unit]]

  # copy() prevents by-reference data.table assignment from modifying the
  # module-level unit_reference object between calls
  reference <- copy(unit_reference)

  # Sort ascending by unit size: top to bottom goes from one drop to the
  # annual output of every river on Earth
  setkey(reference, cubic_metres)

  # equivalent:     how many of each reference unit does the input volume fill?
  # equivalent_fmt: formatted string for display (see format_equivalent())
  # input:          carried through for traceability in downstream outputs
  reference[, equivalent     := value_m3 / cubic_metres]
  reference[, equivalent_fmt := format_equivalent(equivalent)]
  reference[, input          := paste(value, unit)]

  # Separate static volumes from flux-derived units at the point of return;
  # the caller should not need to remember to filter
  list(
    volumes = reference[flux == FALSE, .(display_name, category, cubic_metres,
                                         equivalent, equivalent_fmt, input, plural)],
    flux    = reference[flux == TRUE,  .(display_name, category, cubic_metres,
                                         equivalent, equivalent_fmt, input, plural)]
  )
}


# -----------------------------------------------------------------------------
# nearest_units()
#
# Finds the N reference units whose size is closest to the input volume in
# log space, returning a plain-English description suitable for report writing.
# Log-space distance prevents extreme units (minim, Dead Sea) dominating by
# raw gap when the input sits in a middle range.
#
# Arguments:
#   value  numeric; the volume to describe
#   unit   character; input unit, must be one of names(input_unit_factors)
#   n      integer; number of nearest units to return (default 3)
#
# Returns:
#   character string, e.g.
#   "124 megalitre is approximately equivalent to: 1.00 Rutland Waters, ..."
# -----------------------------------------------------------------------------
nearest_units <- function(value, unit, n = 3L) {

  unit <- match.arg(unit, names(input_unit_factors))

  if (!is.numeric(value) || length(value) != 1 || value <= 0)
    stop("`value` must be a single positive number.")

  # Flux and SI units excluded: see make_water_table() for why.
  # Apothecary units excluded from nearest-match: a hogshead is a fine unit
  # for amusement but "3.44e+06 Hogsheads" is not a useful briefing comparison.
  # The full table via make_water_table() retains them for completeness.
  tbl      <- make_water_table(value, unit)$volumes
  tbl      <- tbl[!category %in% c("SI", "Apothecary")]
  value_m3 <- value * input_unit_factors[[unit]]

  # Distance in log10 space: prevents units at extreme ends of the scale
  # (minim, Dead Sea) from always dominating by raw arithmetic gap.
  # A 1 GL input is equidistant in log space from 100 ML and 10 GL,
  # which is the right behaviour.
  tbl[, log_distance := abs(log10(value_m3) - log10(cubic_metres))]
  setorder(tbl, log_distance)

  # Readability floor: equivalents below 0.01 are technically correct but
  # convey nothing useful ("0.008 Rutland Waters"). Filter before selecting
  # top n so the returned comparisons are all graspable by a reader.
  # If fewer than n units survive the floor, return however many do.
  tbl <- tbl[equivalent >= 0.01]
  nearest <- head(tbl, n)

  # Pluralisation: use the explicit plural from the reference table where
  # one is defined (handles irregulars like Acre-feet). Otherwise apply the
  # default rule: append 's' for any equivalent outside the range [0.95, 1.05],
  # which avoids "1.01 Rutland Waters" while catching "1.36 Sydharbs".
  descriptions <- nearest[, {
    label <- ifelse(
      !is.na(plural),
      ifelse(abs(equivalent - 1) < 0.05, display_name, plural),
      ifelse(abs(equivalent - 1) >= 0.05, paste0(display_name, "s"), display_name)
    )
    paste0(equivalent_fmt, " ", label)
  }]

  # Label uses the supplied value and unit for readability; the caller
  # already knows the volume in metric terms
  paste0(value, " ", unit, " is approximately equivalent to: ",
         paste(descriptions, collapse = ", "), ".")
}


# -----------------------------------------------------------------------------
# rainfall_volume()
#
# Calculates the total volume of water from a rainfall depth over a catchment,
# then passes the result to make_water_table() for unit comparison.
#
# Gross rainfall volume only: depth x area, no losses. For net runoff, supply
# a runoff_coefficient explicitly; the default of 1 makes the assumption clear.
#
# Unit identity: 1 mm x 1 km2 = 1e-3 m x 1e6 m2 = 1e3 m3 = 1 ML. Exact.
#
# Arguments:
#   rainfall_mm          numeric; rainfall depth in millimetres
#   area_km2             numeric; catchment area in square kilometres
#   runoff_coefficient   numeric in (0, 1]; fraction of rainfall becoming
#                        runoff. Default 1 (gross rainfall).
#
# Returns:
#   named list as from make_water_table(), with an additional element:
#   $summary  single-row data.table recording inputs and derived volume
# -----------------------------------------------------------------------------
rainfall_volume <- function(rainfall_mm,
                            area_km2,
                            runoff_coefficient = 1) {

  if (!is.numeric(rainfall_mm) || length(rainfall_mm) != 1 || rainfall_mm <= 0)
    stop("`rainfall_mm` must be a single positive number.")
  if (!is.numeric(area_km2) || length(area_km2) != 1 || area_km2 <= 0)
    stop("`area_km2` must be a single positive number.")
  if (!is.numeric(runoff_coefficient) || length(runoff_coefficient) != 1 ||
      runoff_coefficient <= 0 || runoff_coefficient > 1)
    stop("`runoff_coefficient` must be a single value in (0, 1].")

  # 1 mm x 1 km² = 1e-3 m x 1e6 m² = 1e3 m³: exact, no approximation
  volume_m3 <- rainfall_mm * area_km2 * 1e3 * runoff_coefficient

  # Retain all inputs in the summary so the result is self-documenting
  # when passed downstream to reservoir_fill() or nearest_units()
  summary_dt <- data.table(
    rainfall_mm        = rainfall_mm,
    area_km2           = area_km2,
    runoff_coefficient = runoff_coefficient,
    volume_m3          = volume_m3,
    volume_GL          = volume_m3 / 1e6   # GL for human readability
  )

  # Append $summary to the standard make_water_table() list structure
  # so the result is directly usable by reservoir_fill() and nearest_units()
  result         <- make_water_table(volume_m3, "m3")
  result$summary <- summary_dt

  result
}


# -----------------------------------------------------------------------------
# nearest_units_from_result()
#
# Convenience wrapper: calls nearest_units() directly from a rainfall_volume()
# result, avoiding the need to extract volume_m3 manually.
#
# Arguments:
#   result  named list as returned by rainfall_volume()
#   n       integer; number of nearest units to return (default 3)
#
# Returns:
#   character string as from nearest_units()
# -----------------------------------------------------------------------------
nearest_units_from_result <- function(result, n = 3L) {
  # Pass volume_GL and "gigalitre" for a readable label in the output string;
  # passing raw m3 produces "6.8e+08 m3 is approximately equivalent to..."
  # which is true but harder to read than "680 gigalitre is approximately..."
  nearest_units(result$summary$volume_GL, "gigalitre", n = n)
}


# -----------------------------------------------------------------------------
# reservoir_fill()
#
# Expresses a given volume as a fill fraction of UK reservoirs. Useful for
# contextualising rainfall event volumes or storage anomalies in briefings.
#
# Returns a named list with two data.tables:
#   $partial   reservoirs the volume would partially fill (fill_pct <= 100),
#              sorted ascending by fill percentage; filtered further by threshold
#   $overfill  reservoirs the volume would exceed, sorted descending by the
#              multiple of capacity (i.e. how many times over it would fill them)
#
# A volume that overfills Bewl Water three times over is a more striking
# comparison than knowing it fills Kielder to 4%; both are returned so the
# caller can choose which framing fits the output.
#
# Arguments:
#   volume_m3   numeric; volume in cubic metres
#   reservoirs  character vector of reservoir names, or "all" (default)
#   threshold   numeric in (0, 1]; upper bound on fill_pct for the $partial
#               table. Default 1 (return all partial fills up to 100%).
#
# Each data.table contains:
#   name          reservoir name
#   region        EA/administrative region
#   capacity_GL   gross capacity in gigalitres
#   fill_pct      fill percentage to 1 d.p. (may exceed 100 in $overfill)
#   times_over    ($overfill only) how many times the volume exceeds capacity,
#                 to 2 d.p.; e.g. 3.42 means the volume fills it 3.42 times
#   description   plain-English string for direct use in briefing text
# -----------------------------------------------------------------------------
reservoir_fill <- function(volume_m3, reservoirs = "all", threshold = 1) {

  if (!is.numeric(volume_m3) || length(volume_m3) != 1 || volume_m3 <= 0)
    stop("`volume_m3` must be a single positive number.")
  if (!is.numeric(threshold) || threshold <= 0 || threshold > 1)
    stop("`threshold` must be in (0, 1].")

  # copy() prevents modification of the module-level uk_reservoirs object
  ref <- copy(uk_reservoirs)

  if (!identical(reservoirs, "all")) {
    # Check all requested names exist before subsetting; fail loudly on mismatch
    matched <- reservoirs %in% ref$name
    if (!all(matched))
      stop("Unrecognised reservoir(s): ",
           paste(reservoirs[!matched], collapse = ", "))
    ref <- ref[name %in% reservoirs]
  }

  ref[, fill_fraction := volume_m3 / capacity_m3]
  ref[, fill_pct      := round(fill_fraction * 100, 1)]

  # Split into partial fills and overfills before building descriptions,
  # since the two groups warrant different plain-English framings
  partial  <- ref[fill_fraction <= threshold]
  overfill <- ref[fill_fraction > 1]

  # Partial: "24.2% of Bewl Water (31 GL)"
  partial[, description := paste0(
    fill_pct, "% of ", name, " (", capacity_GL, " GL)"
  )]

  # Overfill: "would fill Bewl Water 3.42 times over (31 GL)"
  overfill[, times_over  := round(fill_fraction, 2)]
  overfill[, description := paste0(
    "would fill ", name, " ", times_over, " times over (", capacity_GL, " GL)"
  )]

  # Partial: ascending by fill so largest partial fills appear at the bottom
  # -- the reader scans down to the most impressive comparison
  setorder(partial, fill_fraction)

  # Overfill: descending by times_over so the most extreme cases lead
  setorder(overfill, -times_over)

  list(
    partial  = partial[,  .(name, region, capacity_GL, fill_pct, description)],
    overfill = overfill[, .(name, region, capacity_GL, fill_pct, times_over, description)]
  )
}


# =============================================================================
# Usage examples
# =============================================================================

# -- make_water_table() -------------------------------------------------------

# 1 gigalitre: full table
result <- make_water_table(1, "gigalitre")
result$volumes
result$flux

# 500 megalitres: formatted column only
make_water_table(500, "megalitre")$volumes[, .(display_name, equivalent_fmt)]

# Quarto / knitr output
# knitr::kable(
#   make_water_table(1, "gigalitre")$volumes[, .(display_name, category, equivalent_fmt)],
#   col.names = c("Unit", "Category", "Equivalent")
# )


# -- nearest_units() ----------------------------------------------------------

nearest_units(124, "megalitre")
#> "124 megalitre is approximately equivalent to:
#>  1.00 Rutland Waters, 0.395 Lake Windermeres, 49.6 Olympic swimming pools."


# -- rainfall_volume() --------------------------------------------------------

# 80 mm over 8,500 km2; gross rainfall
result <- rainfall_volume(80, 8500)
result$summary
#>    rainfall_mm area_km2 runoff_coefficient volume_m3 volume_GL
#>             80     8500                  1   6.8e+08       680

result$volumes[, .(display_name, equivalent_fmt)]
nearest_units_from_result(result)
#> "680 GL is approximately equivalent to:
#>  1.36 Sydharbs, 2.17 Lake Windermeres, 5.48 Rutland Waters."

# Same event; 70% runoff
result_runoff <- rainfall_volume(80, 8500, runoff_coefficient = 0.7)
result_runoff$summary$volume_GL
#> [1] 476


# -- reservoir_fill() ---------------------------------------------------------

# Small event: 15 mm over 500 km2 = 7.5 GL; all partial fills, no overfill
result_small <- rainfall_volume(15, 500)
fill_small <- reservoir_fill(result_small$summary$volume_m3)
fill_small$partial    # all reservoirs partially filled
fill_small$overfill   # empty: 7.5 GL does not exceed any reservoir

# Large event: 80 mm over 8,500 km2 = 680 GL; overfills all but Kielder
result_large <- rainfall_volume(80, 8500)
fill_large <- reservoir_fill(result_large$summary$volume_m3)
fill_large$partial    # only Kielder (199 GL) survives as a partial fill
fill_large$overfill   # all smaller reservoirs; sorted by times_over descending

# Threshold: partial fills up to 15% only
fill_small_thresh <- reservoir_fill(result_small$summary$volume_m3, threshold = 0.15)
fill_small_thresh$partial

# Named reservoirs only
fill_named <- reservoir_fill(result_small$summary$volume_m3,
                             reservoirs = c("Kielder Water", "Rutland Water", "Bewl Water"))
fill_named$partial
fill_named$overfill
