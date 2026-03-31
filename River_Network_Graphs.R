# =============================================================================
# River Network Simplification: Large Synthetic Catchment
# =============================================================================
#
# ~75 reaches, 12 gauges, realistic dendritic structure.
# Sections 3-10 are identical to the small example — the algorithm scales
# without modification.
#
# Dependencies: sf, igraph, ggplot2, patchwork
# =============================================================================

library(sf)
library(igraph)
library(ggplot2)
library(patchwork)


# -- 1. Build a large synthetic river network --------------------------------
#
# Structure (roughly modelled on a Nene-scale catchment):
#
#   Main stem:    12 reaches, flowing south through the centre
#   West major:    8 reaches, joins main at reach 4
#   East major:    8 reaches, joins main at reach 7
#   West minor A:  4 reaches, joins west major at reach 3
#   West minor B:  3 reaches, joins west major at reach 6
#   East minor A:  5 reaches, joins east major at reach 3
#   East minor B:  4 reaches, joins east major at reach 6
#   East minor C:  3 reaches, sub-tributary off east minor A
#   NW headstream: 4 reaches, joins main at reach 2
#   SE headstream: 5 reaches, joins main at reach 9
#   Far west:      4 reaches, joins west minor A at reach 2
#   Far east:      5 reaches, joins east major at reach 2
#
# Total: ~75 reaches, 12 confluences, 10 headwaters, 1 outlet

make_reach <- function(coords, id) {
  st_sf(
    reach_id = id,
    geometry = st_sfc(st_linestring(coords), crs = 27700)
  )
}

# Helper: build a chain of reaches along a path defined by waypoints
# Returns a list of sf objects with sequential IDs
make_chain <- function(waypoints, id_prefix, start_id = 1) {
  n <- nrow(waypoints) - 1
  lapply(seq_len(n), \(i) {
    id <- sprintf("%s%02d", id_prefix, start_id + i - 1)
    make_reach(waypoints[i:(i + 1), , drop = FALSE], id)
  })
}

# ---- Main stem (M01-M12): centre of catchment, flowing south ----
main_pts <- matrix(c(
  500, 2200,
  500, 2100,
  505, 2000,
  500, 1900,
  495, 1800,   # confluence: west major joins here
  500, 1700,
  505, 1600,
  500, 1500,   # confluence: east major joins here
  495, 1400,
  500, 1300,   # confluence: SE headstream joins here
  505, 1200,
  500, 1100,
  500, 1000
), ncol = 2, byrow = TRUE)
main_reaches <- make_chain(main_pts, "M")

# ---- NW headstream (NW01-NW04): joins main at M02 endpoint (500,2000) ----
nw_pts <- matrix(c(
  300, 2300,
  340, 2250,
  380, 2150,
  440, 2060,
  505, 2000
), ncol = 2, byrow = TRUE)
nw_reaches <- make_chain(nw_pts, "NW")

# ---- West major (W01-W08): joins main at M04 endpoint (495,1800) ----
w_pts <- matrix(c(
  100, 2100,
  140, 2050,
  180, 2000,
  220, 1960,   # confluence: west minor A joins here
  260, 1920,
  300, 1880,
  340, 1860,   # confluence: west minor B joins here
  400, 1830,
  495, 1800
), ncol = 2, byrow = TRUE)
w_reaches <- make_chain(w_pts, "W")

# ---- West minor A (WA01-WA04): joins west major at W03 endpoint (220,1960) ----
wa_pts <- matrix(c(
  50, 2200,
  80, 2150,
  130, 2060,   # confluence: far west joins here
  180, 2010,
  220, 1960
), ncol = 2, byrow = TRUE)
wa_reaches <- make_chain(wa_pts, "WA")

# ---- Far west (FW01-FW04): joins west minor A at WA02 endpoint (130,2060) ----
fw_pts <- matrix(c(
  20, 2350,
  30, 2300,
  50, 2220,
  80, 2140,
  130, 2060
), ncol = 2, byrow = TRUE)
fw_reaches <- make_chain(fw_pts, "FW")

# ---- West minor B (WB01-WB03): joins west major at W06 endpoint (340,1860) ----
wb_pts <- matrix(c(
  200, 1950,
  240, 1920,
  290, 1890,
  340, 1860
), ncol = 2, byrow = TRUE)
wb_reaches <- make_chain(wb_pts, "WB")

# ---- East major (E01-E08): joins main at M07 endpoint (500,1500) ----
e_pts <- matrix(c(
  900, 1900,
  870, 1850,
  840, 1800,   # confluence: far east joins here
  800, 1750,   # confluence: east minor A joins here
  750, 1700,
  700, 1650,
  660, 1600,   # confluence: east minor B joins here
  580, 1550,
  500, 1500
), ncol = 2, byrow = TRUE)
e_reaches <- make_chain(e_pts, "E")

# ---- Far east (FE01-FE05): joins east major at E02 endpoint (840,1800) ----
fe_pts <- matrix(c(
  980, 2100,
  960, 2050,
  940, 1980,
  920, 1920,
  880, 1860,
  840, 1800
), ncol = 2, byrow = TRUE)
fe_reaches <- make_chain(fe_pts, "FE")

# ---- East minor A (EA01-EA05): joins east major at E03 endpoint (800,1750) ----
ea_pts <- matrix(c(
  950, 1750,
  930, 1760,
  900, 1780,   # confluence: east minor C joins here
  870, 1770,
  840, 1760,
  800, 1750
), ncol = 2, byrow = TRUE)
ea_reaches <- make_chain(ea_pts, "EA")

# ---- East minor C (EC01-EC03): joins east minor A at EA02 endpoint (900,1780) ----
ec_pts <- matrix(c(
  960, 1850,
  940, 1820,
  920, 1800,
  900, 1780
), ncol = 2, byrow = TRUE)
ec_reaches <- make_chain(ec_pts, "EC")

# ---- East minor B (EB01-EB04): joins east major at E06 endpoint (660,1600) ----
eb_pts <- matrix(c(
  780, 1550,
  750, 1560,
  720, 1570,
  690, 1580,
  660, 1600
), ncol = 2, byrow = TRUE)
eb_reaches <- make_chain(eb_pts, "EB")

# ---- SE headstream (SE01-SE05): joins main at M09 endpoint (500,1300) ----
se_pts <- matrix(c(
  700, 1150,
  670, 1180,
  640, 1220,
  600, 1260,
  550, 1280,
  500, 1300
), ncol = 2, byrow = TRUE)
se_reaches <- make_chain(se_pts, "SE")

# Combine all reaches
reaches <- do.call(rbind, c(
  main_reaches, nw_reaches, w_reaches, wa_reaches, fw_reaches,
  wb_reaches, e_reaches, fe_reaches, ea_reaches, ec_reaches,
  eb_reaches, se_reaches
))

cat("Built", nrow(reaches), "reaches\n")


# -- 2. Create synthetic gauges ---------------------------------------------
#
# 12 gauges distributed across the network at operationally realistic
# locations: main stem (upper, mid, lower, outlet), major tribs, minor tribs.

gauge_data <- data.frame(
  gauge_id   = sprintf("G%03d", 1:12),
  gauge_name = c(
    "Main Head",       # near top of main stem
    "Main Upper",      # above west confluence
    "Main Mid",        # between west and east confluences
    "Main Lower",      # below east confluence
    "Outlet",          # bottom of main stem
    "West Upper",      # upper west major
    "West Lower",      # lower west major
    "NW Stream",       # NW headstream
    "East Upper",      # upper east major
    "East Lower",      # lower east major
    "East Minor A",    # east minor A
    "SE Stream"        # SE headstream
  ),
  reach_id = c(
    "M01",   # Main Head
    "M03",   # Main Upper
    "M06",   # Main Mid
    "M09",   # Main Lower
    "M12",   # Outlet
    "W02",   # West Upper
    "W07",   # West Lower
    "NW02",  # NW Stream
    "E02",   # East Upper
    "E07",   # East Lower
    "EA03",  # East Minor A
    "SE03"   # SE Stream
  ),
  stringsAsFactors = FALSE
)

gauge_points <- lapply(seq_len(nrow(gauge_data)), \(i) {
  rid <- gauge_data$reach_id[i]
  geom <- reaches$geometry[reaches$reach_id == rid]
  midpt <- st_point_on_surface(geom)
  st_sf(
    gauge_id   = gauge_data$gauge_id[i],
    gauge_name = gauge_data$gauge_name[i],
    reach_id   = gauge_data$reach_id[i],
    geometry   = midpt,
    crs        = 27700
  )
})
gauges <- do.call(rbind, gauge_points)

cat("Placed", nrow(gauges), "gauges\n")


# =============================================================================
# SECTIONS 3-10: IDENTICAL TO THE SMALL EXAMPLE
# The algorithm works the same regardless of network size.
# =============================================================================


# -- 3. Build the full directed graph from reach endpoints -------------------

get_endpoints <- function(geom) {
  cc <- st_coordinates(geom)
  list(
    from = cc[1, c("X", "Y")],
    to   = cc[nrow(cc), c("X", "Y")]
  )
}

make_node_id <- function(xy, digits = 0) {
  paste0(round(xy[["X"]], digits), "_", round(xy[["Y"]], digits))
}

endpoints <- lapply(st_geometry(reaches), get_endpoints)

edge_df <- data.frame(
  from     = vapply(endpoints, \(e) make_node_id(e$from), character(1)),
  to       = vapply(endpoints, \(e) make_node_id(e$to),   character(1)),
  reach_id = reaches$reach_id,
  stringsAsFactors = FALSE
)

g_full <- graph_from_data_frame(edge_df, directed = TRUE)

cat("Full network:", vcount(g_full), "nodes,", ecount(g_full), "edges\n")


# -- 4. Identify key nodes: confluences + gauge locations --------------------

node_deg <- degree(g_full, mode = "all")
confluences <- names(node_deg[node_deg > 2])

# Place gauges at the UPSTREAM (from) end of their reach.
# Using the downstream (to) end risks landing on a confluence node, which
# deduplicates in `keep` and makes the gauge absorb the confluence —
# tributaries then appear to flow into the gauge rather than meeting at
# a junction downstream.
gauge_nodes <- vapply(gauge_data$reach_id, \(rid) {
  row <- which(edge_df$reach_id == rid)
  if (length(row) == 0) return(NA_character_)
  edge_df$from[row[1]]
}, character(1))
names(gauge_nodes) <- gauge_data$gauge_name

headwaters <- names(which(degree(g_full, mode = "in") == 0))
outlets    <- names(which(degree(g_full, mode = "out") == 0))

# If a gauge lands on a headwater node, that's fine — it keeps both roles.
# But if a gauge lands on a confluence, split them: nudge the gauge one
# reach upstream so both the gauge and the confluence appear in the
# simplified graph. (With from-placement this is rare but possible if
# the gauge reach starts at a confluence.)
gauge_on_confluence <- gauge_nodes[gauge_nodes %in% confluences]
if (length(gauge_on_confluence) > 0) {
  warning(
    "Gauge(s) share a node with a confluence: ",
    paste(names(gauge_on_confluence), collapse = ", "),
    "\n  Both will appear but the gauge may sit at the confluence level."
  )
}

keep <- unique(c(confluences, gauge_nodes, headwaters, outlets))

cat("Keeping", length(keep), "nodes:",
    length(confluences), "confluences,",
    sum(!is.na(gauge_nodes)), "gauges,",
    length(headwaters), "headwaters,",
    length(outlets), "outlets\n")


# -- 5. Contract the graph --------------------------------------------------

simplified_edges <- list()

for (node in keep) {
  out_nbs <- neighbors(g_full, node, mode = "out")
  if (length(out_nbs) == 0) next
  
  for (i in seq_along(out_nbs)) {
    current <- V(g_full)$name[out_nbs[i]]
    n_reaches <- 1L
    
    while (!(current %in% keep)) {
      out_next <- neighbors(g_full, current, mode = "out")
      if (length(out_next) == 0) {
        keep <- c(keep, current)
        break
      }
      current <- V(g_full)$name[out_next[1]]
      n_reaches <- n_reaches + 1L
    }
    
    simplified_edges <- c(simplified_edges, list(data.frame(
      from      = node,
      to        = current,
      n_reaches = n_reaches,
      stringsAsFactors = FALSE
    )))
  }
}

simp_df <- do.call(rbind, simplified_edges)
g_simple <- graph_from_data_frame(simp_df, directed = TRUE)

# Tag node types
V(g_simple)$type  <- "other"
V(g_simple)$type[V(g_simple)$name %in% confluences]  <- "confluence"
V(g_simple)$type[V(g_simple)$name %in% gauge_nodes]  <- "gauge"
V(g_simple)$type[V(g_simple)$name %in% headwaters]   <- "headwater"
V(g_simple)$type[V(g_simple)$name %in% outlets]       <- "outlet"

# Build a lookup for gauge labels
gauge_lookup <- setNames(
  paste0(gauge_data$gauge_id, "\n", gauge_data$gauge_name),
  gauge_nodes
)

V(g_simple)$label <- vapply(V(g_simple)$name, \(nm) {
  if (nm %in% names(gauge_lookup)) return(gauge_lookup[[nm]])
  type <- V(g_simple)$type[V(g_simple)$name == nm]
  if (type == "confluence") return("Confluence")
  if (type == "headwater")  return("Headwater")
  if (type == "outlet")     return("Outlet")
  return(nm)
}, character(1))

cat("Simplified:", vcount(g_simple), "nodes,", ecount(g_simple), "edges\n")


# -- 6. Custom hierarchical layout -------------------------------------------

node_names <- V(g_simple)$name
topo_order <- topo_sort(g_simple, mode = "out")

# Depth: longest path from any headwater
depth <- setNames(rep(0L, length(node_names)), node_names)
for (v in topo_order) {
  vname <- V(g_simple)$name[v]
  out_nbs <- neighbors(g_simple, v, mode = "out")
  for (j in seq_along(out_nbs)) {
    nb_name <- V(g_simple)$name[out_nbs[j]]
    depth[nb_name] <- max(depth[nb_name], depth[vname] + 1L)
  }
}

# Subtree width for horizontal spacing
subtree_width <- setNames(rep(1L, length(node_names)), node_names)
for (v in rev(topo_order)) {
  vname <- V(g_simple)$name[v]
  in_nbs <- neighbors(g_simple, v, mode = "in")
  if (length(in_nbs) > 0) {
    subtree_width[vname] <- sum(vapply(seq_along(in_nbs), \(j) {
      subtree_width[V(g_simple)$name[in_nbs[j]]]
    }, integer(1)))
  }
}

# Recursive x-position assignment from the outlet upward
x_pos <- setNames(rep(NA_real_, length(node_names)), node_names)

assign_x <- function(node, x_left, x_right) {
  x_pos[node] <<- (x_left + x_right) / 2
  in_nbs <- neighbors(g_simple, node, mode = "in")
  if (length(in_nbs) == 0) return(invisible(NULL))
  
  in_names <- vapply(
    seq_along(in_nbs),
    \(j) V(g_simple)$name[in_nbs[j]],
    character(1)
  )
  widths  <- subtree_width[in_names]
  total_w <- sum(widths)
  
  cursor <- x_left
  for (k in seq_along(in_names)) {
    frac <- widths[k] / total_w
    child_right <- cursor + frac * (x_right - x_left)
    assign_x(in_names[k], cursor, child_right)
    cursor <- child_right
  }
}

outlet_node <- node_names[V(g_simple)$type == "outlet"][1]
assign_x(outlet_node, 0, 1)

# Layout data frame
layout_df <- data.frame(
  name  = node_names,
  x     = x_pos[node_names],
  y     = -depth[node_names],
  type  = V(g_simple)$type,
  label = V(g_simple)$label,
  stringsAsFactors = FALSE
)

# -- Post-layout spacing pass ------------------------------------------------
# Enforce a minimum horizontal gap between boxes on the same row.
# The subtree-width layout can pack nodes too tightly in narrow branches.
# We sort each row by x and push neighbours apart if they overlap.

min_gap <- 0.14  # minimum centre-to-centre distance (roughly 2 * box_hw + padding)

for (row_y in unique(layout_df$y)) {
  idx <- which(layout_df$y == row_y)
  if (length(idx) <= 1) next
  
  # Sort by current x
  idx <- idx[order(layout_df$x[idx])]
  
  # Iterative push: sweep left to right, nudging rightward if too close
  for (j in 2:length(idx)) {
    gap <- layout_df$x[idx[j]] - layout_df$x[idx[j - 1]]
    if (gap < min_gap) {
      shift <- (min_gap - gap) / 2
      layout_df$x[idx[j]]     <- layout_df$x[idx[j]]     + shift
      layout_df$x[idx[j - 1]] <- layout_df$x[idx[j - 1]] - shift
    }
  }
}

# Rescale x to [0.05, 0.95] so boxes don't clip the plot edges
x_range <- range(layout_df$x)
if (diff(x_range) > 0) {
  layout_df$x <- 0.05 + 0.9 * (layout_df$x - x_range[1]) / diff(x_range)
}

# Edge geometry for the schematic
edge_geom <- do.call(rbind, lapply(seq_len(ecount(g_simple)), \(i) {
  e <- ends(g_simple, i)
  fr <- layout_df[layout_df$name == e[1], ]
  to <- layout_df[layout_df$name == e[2], ]
  data.frame(
    x = fr$x, y = fr$y, xend = to$x, yend = to$y,
    n_reaches = E(g_simple)$n_reaches[i],
    stringsAsFactors = FALSE
  )
}))


# -- 7. Colour palette -------------------------------------------------------

type_colours <- c(
  gauge      = "#D85A30",
  confluence = "#5F5E5A",
  headwater  = "#639922",
  outlet     = "#1D9E75",
  other      = "#888780"
)

type_fills <- c(
  gauge      = "#FAECE7",
  confluence = "#F1EFE8",
  headwater  = "#EAF3DE",
  outlet     = "#E1F5EE",
  other      = "#F1EFE8"
)


# -- 8. Plot: spatial view ---------------------------------------------------

p_spatial <- ggplot() +
  geom_sf(data = reaches, colour = "steelblue", linewidth = 0.5) +
  geom_sf(data = gauges, colour = "#D85A30", fill = "#FAECE7",
          shape = 21, size = 3, stroke = 1) +
  geom_sf_text(data = gauges, aes(label = gauge_name),
               nudge_x = 20, hjust = 0, size = 2.2, colour = "grey30") +
  labs(title = "Spatial view",
       subtitle = paste(nrow(reaches), "reaches,", nrow(gauges), "gauges")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text     = element_blank(),
    axis.title    = element_blank(),
    panel.grid    = element_line(colour = "grey95"),
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(colour = "grey50", size = 8)
  )


# -- 9. Plot: schematic diagram ----------------------------------------------

# Scale box sizes to the number of nodes
n_nodes <- nrow(layout_df)
max_depth <- max(depth)

# Adaptive sizing: shrink boxes as the network grows
box_hw <- min(0.08, 0.6 / max(table(layout_df$y)))
box_hh <- min(0.15, 0.8 / (max_depth + 1))

# L-bend edges
edge_geom$ymid <- (edge_geom$y + edge_geom$yend) / 2

p_schema <- ggplot() +
  # Edge segment 1: vertical drop from source
  geom_segment(
    data = edge_geom,
    aes(x = x, y = y - box_hh, xend = x, yend = ymid),
    colour = "steelblue", linewidth = 0.4
  ) +
  # Edge segment 2: horizontal jog
  geom_segment(
    data = edge_geom,
    aes(x = x, y = ymid, xend = xend, yend = ymid),
    colour = "steelblue", linewidth = 0.4
  ) +
  # Edge segment 3: vertical drop into target (with arrow)
  geom_segment(
    data = edge_geom,
    aes(x = xend, y = ymid, xend = xend, yend = yend + box_hh),
    colour = "steelblue", linewidth = 0.4,
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
  # Reach-count badge on each edge
  geom_label(
    data = edge_geom,
    aes(
      x = ifelse(x == xend, x + 0.012, (x + xend) / 2),
      y = ymid,
      label = n_reaches
    ),
    size = 1.8, label.size = 0.2, label.r = unit(1.5, "pt"),
    fill = "white", colour = "grey50",
    label.padding = unit(1.5, "pt")
  ) +
  # Node boxes
  geom_rect(
    data = layout_df,
    aes(
      xmin = x - box_hw, xmax = x + box_hw,
      ymin = y - box_hh, ymax = y + box_hh,
      fill = type, colour = type
    ),
    linewidth = 0.5, show.legend = FALSE
  ) +
  # Node labels
  geom_text(
    data = layout_df,
    aes(x = x, y = y, label = label),
    size = 1.8, fontface = "bold", lineheight = 0.8,
    colour = "grey20"
  ) +
  scale_fill_manual(values = type_fills) +
  scale_colour_manual(values = type_colours) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Simplified schematic",
    subtitle = paste0(
      n_nodes, " key nodes from ", nrow(reaches), " reaches  |  ",
      "edge labels = collapsed reach count"
    )
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(colour = "grey50", size = 8),
    plot.margin   = margin(10, 15, 10, 15)
  )


# -- 10. Combine and save ---------------------------------------------------

p_combined <- p_spatial + p_schema +
  plot_layout(widths = c(1, 1.5)) +
  plot_annotation(
    title    = "River network simplification",
    subtitle = paste(nrow(reaches), "reaches collapsed to",
                     n_nodes, "key nodes"),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey50", size = 9)
    )
  )

ggsave("river_network_large.png", p_combined,
       width = 16, height = 10, dpi = 200, bg = "white")

cat("\nDone. Saved to river_network_large.png\n")
cat("Summary:\n")
cat("  Full network:", vcount(g_full), "nodes,", ecount(g_full), "edges\n")
cat("  Simplified:  ", vcount(g_simple), "nodes,", ecount(g_simple), "edges\n")
cat("  Confluences: ", length(confluences), "\n")
cat("  Gauges:      ", sum(!is.na(gauge_nodes)), "\n")
cat("  Headwaters:  ", length(headwaters), "\n")
cat("  Max depth:   ", max_depth, "levels\n")
