# =============================================================================
# River Network Simplification: From Spatial Reaches to Schematic Diagram
# =============================================================================
#
# Demonstrates building a simplified network diagram from a river shapefile,
# retaining only gauge locations and confluences.
#
# Dependencies: sf, igraph, ggplot2, ggraph, patchwork
# =============================================================================

library(sf)
library(igraph)
library(ggplot2)
library(ggraph)
library(patchwork)

# -- 1. Build a synthetic river network --------------------------------------
#
# We construct a dendritic network manually: a main stem with two tributaries,
# each broken into multiple reaches. Think of it as a simplified version of
# a real river system with ~15 reaches.

make_reach <- function(coords, id) {
  st_sf(
    reach_id = id,
    geometry = st_sfc(st_linestring(coords), crs = 27700)
  )
}

# Main stem: 5 reaches flowing south (high Y to low Y)
main <- list(
  make_reach(matrix(c(400, 1000,  400, 900), ncol = 2, byrow = TRUE), "R01"),
  make_reach(matrix(c(400, 900,   400, 800), ncol = 2, byrow = TRUE), "R02"),
  make_reach(matrix(c(400, 800,   400, 700), ncol = 2, byrow = TRUE), "R03"),
  make_reach(matrix(c(400, 700,   400, 600), ncol = 2, byrow = TRUE), "R04"),
  make_reach(matrix(c(400, 600,   400, 500), ncol = 2, byrow = TRUE), "R05")
)

# Western tributary: joins main stem at (400, 800) — the top of R03
west_trib <- list(
  make_reach(matrix(c(200, 950,  250, 900), ncol = 2, byrow = TRUE), "R06"),
  make_reach(matrix(c(250, 900,  300, 860), ncol = 2, byrow = TRUE), "R07"),
  make_reach(matrix(c(300, 860,  350, 830), ncol = 2, byrow = TRUE), "R08"),
  make_reach(matrix(c(350, 830,  400, 800), ncol = 2, byrow = TRUE), "R09")
)

# Eastern tributary: joins main stem at (400, 700) — the top of R04
east_trib <- list(
  make_reach(matrix(c(600, 900,  550, 850), ncol = 2, byrow = TRUE), "R10"),
  make_reach(matrix(c(550, 850,  500, 800), ncol = 2, byrow = TRUE), "R11"),
  make_reach(matrix(c(500, 800,  450, 750), ncol = 2, byrow = TRUE), "R12"),
  make_reach(matrix(c(450, 750,  400, 700), ncol = 2, byrow = TRUE), "R13")
)

# Small sub-tributary off the eastern tributary at (550, 850)
sub_trib <- list(
  make_reach(matrix(c(650, 920,  620, 890), ncol = 2, byrow = TRUE), "R14"),
  make_reach(matrix(c(620, 890,  550, 850), ncol = 2, byrow = TRUE), "R15")
)

reaches <- do.call(rbind, c(main, west_trib, east_trib, sub_trib))


# -- 2. Create synthetic gauges ---------------------------------------------
#
# Place gauges on selected reaches. In real data these come from your
# gauge metadata with a reach_id column.

gauge_data <- data.frame(
  gauge_id  = c("G001", "G002", "G003", "G004", "G005"),
  gauge_name = c("Oundle Main", "West Arm Upper", "East Arm",
                 "Sub Trib Head", "Outlet"),
  reach_id  = c("R02", "R07", "R11", "R14", "R05"),
  stringsAsFactors = FALSE
)

# Place each gauge at the midpoint of its reach for display
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


# -- 3. Build the full directed graph from reach endpoints -------------------

get_endpoints <- function(geom) {
  cc <- st_coordinates(geom)
  list(
    from = cc[1, c("X", "Y")],
    to   = cc[nrow(cc), c("X", "Y")]
  )
}

# Node IDs from coordinates (rounded to avoid floating-point drift)
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

# Confluences: nodes where degree (in + out combined) > 2
node_deg <- degree(g_full, mode = "all")
confluences <- names(node_deg[node_deg > 2])

# Map gauges to their downstream node (the 'to' end of the reach)
gauge_nodes <- vapply(gauge_data$reach_id, \(rid) {
  row <- which(edge_df$reach_id == rid)
  if (length(row) == 0) return(NA_character_)
  edge_df$to[row[1]]
}, character(1))
names(gauge_nodes) <- gauge_data$gauge_name

# Also keep headwater nodes (in-degree == 0) and the outlet (out-degree == 0)
headwaters <- names(which(degree(g_full, mode = "in") == 0))
outlets    <- names(which(degree(g_full, mode = "out") == 0))

keep <- unique(c(confluences, gauge_nodes, headwaters, outlets))

cat("Keeping", length(keep), "nodes:",
    length(confluences), "confluences,",
    sum(!is.na(gauge_nodes)), "gauges,",
    length(headwaters), "headwaters,",
    length(outlets), "outlets\n")


# -- 5. Contract the graph: walk between key nodes --------------------------
#
# For each key node, follow outgoing edges through non-key nodes until
# we reach another key node. Record the simplified edge and the reach
# count it represents.

# -- 5. Contract the graph: walk between key nodes --------------------------

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

# Tag node types for styling
V(g_simple)$type <- "other"
V(g_simple)$type[V(g_simple)$name %in% confluences] <- "confluence"
V(g_simple)$type[V(g_simple)$name %in% gauge_nodes] <- "gauge"
V(g_simple)$type[V(g_simple)$name %in% headwaters]  <- "headwater"
V(g_simple)$type[V(g_simple)$name %in% outlets]      <- "outlet"

# Attach gauge names as labels where applicable
V(g_simple)$label <- ""
for (i in seq_along(gauge_nodes)) {
  idx <- which(V(g_simple)$name == gauge_nodes[i])
  if (length(idx) > 0) {
    V(g_simple)$label[idx] <- names(gauge_nodes)[i]
  }
}

# Label confluences
for (cn in confluences) {
  idx <- which(V(g_simple)$name == cn)
  if (length(idx) > 0 && V(g_simple)$label[idx] == "") {
    V(g_simple)$label[idx] <- "Confluence"
  }
}

cat("Simplified network:", vcount(g_simple), "nodes,", ecount(g_simple), "edges\n")


# -- 6. Plot both views side by side ----------------------------------------

# 6a. Spatial view: reaches + gauges
p_spatial <- ggplot() +
  geom_sf(data = reaches, colour = "steelblue", linewidth = 0.8) +
  geom_sf(data = gauges, colour = "red", size = 3) +
  geom_sf_text(data = gauges, aes(label = gauge_name),
               nudge_x = 40, hjust = 0, size = 3) +
  labs(title = "Spatial View", subtitle = "All 15 reaches with 5 gauges") +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = "grey95")
  )

# 6b. Schematic diagram using ggraph
# Sugiyama layout gives a top-to-bottom directed-graph arrangement
p_schema <- ggraph(g_simple, layout = "sugiyama") +
  geom_edge_link(
    aes(label = n_reaches),
    arrow       = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap     = circle(4, "mm"),
    colour      = "steelblue",
    linewidth   = 0.8,
    angle_calc  = "along",
    label_dodge = unit(3, "mm"),
    label_size  = 2.5,
    label_colour = "grey50"
  ) +
  geom_node_point(aes(colour = type, shape = type), size = 4) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3,
                 max.overlaps = 20) +
  scale_colour_manual(values = c(
    gauge      = "red",
    confluence = "grey30",
    headwater  = "forestgreen",
    outlet     = "darkorange",
    other      = "grey60"
  )) +
  scale_shape_manual(values = c(
    gauge      = 16,
    confluence = 18,
    headwater  = 17,
    outlet     = 15,
    other      = 1
  )) +
  labs(title = "Simplified Schematic",
       subtitle = "Only gauges, confluences, headwaters & outlet") +
  theme_void() +
  theme(legend.position = "bottom")

# Combine
p_combined <- p_spatial + p_schema +
  plot_layout(widths = c(1, 1.2)) +
  plot_annotation(
    title = "River Network Simplification",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("river_network_simplified.png", p_combined,
       width = 14, height = 8, dpi = 150, bg = "white")

cat("\nDone. Saved to river_network_simplified.png\n")
