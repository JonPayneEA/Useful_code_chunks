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

Feel free to contribute and add more snippets or improve the existing ones!
