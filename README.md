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

**Dependencies:** `ggplot2`, `dplyr`, `scales`

---

### CrossSection_Rating_Dual_Plot.R
Produces two separate, vertically stacked plots combined with `patchwork`:

- **Plot A – Cross-Section:** Channel bed with water-fill polygons and dashed water surface lines at three flow stages, with inline stage labels
- **Plot B – Rating Curve:** Stage–discharge curve with above-bankfull zone shading, colour change at bankfull, and simulated gauged observation points

**Dependencies:** `ggplot2`, `patchwork`, `dplyr`

---

### RatingCurve_Gap_Check.R
Utilities for detecting and resolving discharge discontinuities ("gaps") at the junctions between independently-fitted rating curve limbs. When limbs are fitted separately their endpoints rarely align exactly, producing visual gaps and hydraulic inconsistencies.

- **detect_rc_gaps():** Scans every limb junction and reports the absolute and relative discharge gap; flags junctions that exceed configurable tolerances. Accepts an explicit limb-ID column or auto-detects limbs from monotonicity breaks in discharge.
- **resolve_rc_gaps():** Closes flagged gaps using one of three strategies:
  - `"interpolate"` *(default)* — averages the two limbs' bridge estimates at the breakpoint stage so both limbs meet at an agreed discharge without distorting either curve's shape
  - `"snap_to_lower"` — shifts the upper limb's starting discharge to match the lower limb's end (use when the lower/gauged limb is trusted)
  - `"snap_to_upper"` — shifts the lower limb's ending discharge to match the upper limb's start (use when the upper limb anchor, e.g. a flood-frequency estimate, is trusted)
- **plot_rc_gaps():** Diagnostic ggplot overlaying the original (dashed) and corrected (solid) curves, coloured by limb. Both curves share the same limb colour scheme; resolved junctions are marked with a filled dot to confirm the corrected limbs meet. Flagged junctions are annotated at the right margin with the junction stage and ΔQ, staggered vertically if close together to prevent overlap. Uses `geom_path` (rather than `geom_line`) so lines are always drawn in stage order — important because gap resolution can make discharge non-monotonic over the first two points of a corrected limb.

**Dependencies:** `ggplot2` (base R otherwise)

---

Feel free to contribute and add more snippets or improve the existing ones!
