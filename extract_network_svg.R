# =============================================================================
# extract_svg_links.R
# 
# Extract directed links and node metadata from a FEWS-style flood-network SVG.
# 
# Portability:
# - Regex uses R 4.0+ raw strings r"(…)".
# - XPath uses intToUtf8(39L) to avoid smart-quote mangling on copy/paste.
# =============================================================================

library(xml2)
library(data.table)
library(igraph)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# – Shared colour/shape lookups used by multiple functions —————––

.class_colours <- c(
Import            = "#d4351c",
GriddedImport     = "#f47738",
CatAvg            = "#fbc9ec",
CatAvgGroundwater = "#e8b0d4",
PDM               = "#92d050",
Groundwater       = "#2b8c2b",
BlackBox          = "#7fcdbb",
ARMA              = "#b7dee8",
FMP               = "#6599d9",
Sum               = "#fcddc4",
AstroSum          = "#f9c49a",
LevelToFlow       = "#c4a6d9",
FlowToLevel       = "#9b7fc4",
DataHierarchy     = "#a6a6a6"
)

.edge_colours <- c(
"rgb(212, 53, 28)"  = "#d4351c",
"rgb(0, 48, 120)"   = "#003078",
"rgb(133, 153, 75)" = "#85994b"
)

.stroke_labels <- c(
"rgb(212, 53, 28)"  = "Rainfall",
"rgb(0, 48, 120)"   = "Flow",
"rgb(133, 153, 75)" = "Level"
)

#’ Extract directed links from a FEWS-style flood-network SVG
#’
#’ Parses the SVG’s `<g id="nodes">` and `<g id="edges">` groups, matching
#’ edge endpoints to node connection ports by Euclidean distance. Returns
#’ human-readable node names, not UUIDs.
#’
#’ @param svg_file Character. Path to the SVG file.
#’ @param tol Numeric. Maximum snap distance (in SVG user units) before an
#’   edge endpoint is flagged as suspect. Default 5.
#’
#’ @return A list with two [data.table]s:
#’   \describe{
#’     \item{edges}{`from_name`, `to_name`, `stroke` (RGB string), `dashed`
#’       (logical). One row per unique directed link.}
#’     \item{nodes}{`node_name`, `node_id` (shared UUID prefix for Imports),
#’       `node_class` (e.g. PDM, FMP, CatAvg, ARMA, Import, Sum,
#’       DataHierarchy).}
#’   }
#’
#’ @examples
#’ result <- extract_svg_links("network_upper_Ouse.svg")
#’ result$edges
#’ result$nodes
#’
#’ @export
extract_svg_links <- function(svg_file, tol = 5) {

doc <- read_xml(svg_file)
ns  <- c(s = "http://www.w3.org/2000/svg")

q        <- intToUtf8(39L)
xp_nodes <- paste0("//s:g[contains(@class,", q, "node", q, ")]")
xp_edges <- paste0("//s:g[@id=", q, "edges", q, "]/s:g")

# – helpers —————————————————————

parse_translate <- function(s) {
if (is.na(s)) return(c(0, 0))
m <- regmatches(s, regexec(r"(translate(([-0-9.]+)[ ,]+([-0-9.]+)))", s))[[1]]
if (length(m) < 3) c(0, 0) else as.numeric(m[2:3])
}

node_label <- function(g) {
tspans <- xml_find_all(g, ".//s:text/s:tspan", ns)
if (length(tspans) == 0) return(NA_character_)
trimws(xml_text(tspans[[length(tspans)]]))
}

strip_node_prefix <- function(cls) sub(r"(^node\s+)", "", cls)
shared_id         <- function(id)  sub("/.*$", "", id)

# – 1. Ports (one row per connection point) —————————––

node_groups <- xml_find_all(doc, xp_nodes, ns)
if (length(node_groups) == 0) stop("No node elements found. Check SVG structure.")

ports <- rbindlist(lapply(node_groups, function(g) {
node_id    <- xml_attr(g, "id")
node_class <- strip_node_prefix(xml_attr(g, "class"))
node_name  <- node_label(g)
tr         <- parse_translate(xml_attr(g, "transform"))


circles <- xml_find_all(g, ".//s:circle", ns)
if (length(circles) > 0) {
  return(data.table(
    node_id    = node_id,
    node_name  = node_name,
    node_class = node_class,
    x = tr[1] + as.numeric(xml_attr(circles, "cx")),
    y = tr[2] + as.numeric(xml_attr(circles, "cy"))
  ))
}

polys <- xml_find_all(g, ".//s:polygon", ns)
if (length(polys) == 0) return(NULL)
pts <- as.numeric(strsplit(xml_attr(polys[[1]], "points"), "[, ]+")[[1]])
xs  <- pts[c(TRUE, FALSE)]
ys  <- pts[c(FALSE, TRUE)]
data.table(
  node_id    = node_id,
  node_name  = node_name,
  node_class = node_class,
  x = tr[1] + mean(xs),
  y = tr[2] + max(ys)
)


}), fill = TRUE)

# – 2. Parse edge endpoints ———————————————–

edge_groups <- xml_find_all(doc, xp_edges, ns)
if (length(edge_groups) == 0) stop("No edges found. Check SVG structure.")

raw_edges <- rbindlist(lapply(seq_along(edge_groups), function(i) {
paths <- xml_find_all(edge_groups[[i]], ".//s:path", ns)
if (length(paths) == 0) return(NULL)


d1 <- xml_attr(paths[[1]], "d")
dN <- xml_attr(paths[[length(paths)]], "d")

start <- as.numeric(
  regmatches(d1, regexec(r"(M\s+(-?[0-9.]+)\s+(-?[0-9.]+))", d1))[[1]][2:3]
)
nums <- as.numeric(regmatches(dN, gregexpr(r"(-?[0-9.]+)", dN))[[1]])
end  <- nums[(length(nums) - 1):length(nums)]

style  <- xml_attr(paths[[1]], "style") %||% ""
stroke <- regmatches(style, regexec(r"(stroke:\s*([^;]+))", style))[[1]][2]

data.table(
  edge_id = i,
  start_x = start[1], start_y = start[2],
  end_x   = end[1],   end_y   = end[2],
  stroke  = trimws(stroke),
  dashed  = grepl("dasharray", style)
)


}))

# – 3. Match endpoints to nearest port ————————————

nearest <- function(x, y) {
d <- sqrt((ports$x - x)^2 + (ports$y - y)^2)
i <- which.min(d)
list(ports$node_name[i], d[i])
}

raw_edges[, c("from_name", "from_dist") := nearest(start_x, start_y), by = edge_id]
raw_edges[, c("to_name", "to_dist")     := nearest(end_x, end_y),     by = edge_id]

n_suspect <- sum(raw_edges$from_dist > tol | raw_edges$to_dist > tol)
if (n_suspect > 0) {
message(n_suspect, " edge(s) did not snap cleanly (dist > ", tol, ")")
}

# – 4. Build outputs ——————————————————

edges <- unique(raw_edges[, .(from_name, to_name, stroke, dashed)])

nodes <- unique(ports[, .(
node_id    = shared_id(first(node_id)),
node_class = first(node_class)
), by = node_name])

list(edges = edges, nodes = nodes)
}

#’ Build an igraph object from extraction results
#’
#’ Shared helper used by the plot functions and [subset_network()]. Constructs
#’ a directed graph with vertex attributes `node_id`, `node_class` and edge
#’ attributes `stroke`, `dashed`.
#’
#’ @param result List returned by [extract_svg_links()].
#’
#’ @return An [igraph] directed graph.
#’
#’ @export
build_graph <- function(result) {
edges_df <- as.data.frame(result$edges)
names(edges_df)[1:2] <- c("from", "to")

graph_from_data_frame(
d        = edges_df,
vertices = as.data.frame(result$nodes[, c("node_name", "node_id", "node_class")]),
directed = TRUE
)
}

#’ Plot the network as a static Sugiyama DAG
#’
#’ Uses [igraph::layout_with_sugiyama()] for a clean layered layout. Nodes
#’ are coloured by class; edges are coloured by gauge type (rainfall, flow,
#’ level).
#’
#’ @param result List returned by [extract_svg_links()] or [subset_network()].
#’ @param label_cex Numeric. Character expansion factor for vertex labels.
#’   Default 0.5.
#’
#’ @return The [igraph] object, invisibly.
#’
#’ @examples
#’ g <- plot_network(result, label_cex = 0.45)
#’
#’ @export
plot_network <- function(result, label_cex = 0.5) {

g <- build_graph(result)

vcols <- unname(.class_colours[V(g)$node_class])
vcols[is.na(vcols)] <- "grey80"

ecols <- unname(.edge_colours[E(g)$stroke])
ecols[is.na(ecols)] <- "grey50"

dashed <- E(g)$dashed
if (is.null(dashed)) dashed <- rep(FALSE, ecount(g))
dashed[is.na(dashed)] <- FALSE
estyle <- ifelse(dashed, 2L, 1L)

lay <- layout_with_sugiyama(g)

plot(g,
layout             = lay$layout,
vertex.label       = V(g)$name,
vertex.label.cex   = label_cex,
vertex.label.color = "black",
vertex.size        = 6,
vertex.color       = vcols,
vertex.frame.color = "grey40",
edge.color         = ecols,
edge.lty           = estyle,
edge.arrow.size    = 0.25,
edge.width         = 0.8,
main               = "Flood Forecast Network")

legend("bottomright",
legend = names(.class_colours),
fill   = .class_colours,
cex    = 0.6,
title  = "Node type")

invisible(g)
}

#’ Plot the network as an interactive visNetwork widget
#’
#’ Builds an [igraph] object, sets visual attributes for nodes and edges,
#’ and converts to a [visNetwork] widget via [visNetwork::visIgraph()].
#’ Layout is computed in R using Sugiyama to avoid vis.js hierarchical
#’ layout issues with cyclic graphs.
#’
#’ Hover over nodes to see name, type, and UUID. Click to highlight
#’ neighbours. Use the dropdown to jump to a node by name.
#’
#’ @param result List returned by [extract_svg_links()] or [subset_network()].
#’ @param title Character. Title shown above the widget. Default
#’   "Flood Forecast Network".
#’
#’ @return A [visNetwork::visNetwork] htmlwidget.
#’
#’ @examples
#’ plot_network_interactive(result)
#’ plot_network_interactive(upstream, title = "Upstream of Bedford Brackley FMP")
#’
#’ @export
plot_network_interactive <- function(result, title = "Flood Forecast Network") {

if (!requireNamespace("visNetwork", quietly = TRUE)) {
stop("Install visNetwork first: install.packages(‘visNetwork’)")
}

g <- build_graph(result)

# – compute layout in R —————————————————

lay <- layout_with_sugiyama(g)$layout
lay[, 1] <- lay[, 1] * 200
lay[, 2] <- lay[, 2] * -150

# – nodes data.frame ——————————————————

vis_shapes <- c(
Import            = "triangle",
GriddedImport     = "triangleDown",
CatAvg            = "dot",
CatAvgGroundwater = "dot",
PDM               = "square",
Groundwater       = "square",
BlackBox          = "square",
ARMA              = "diamond",
FMP               = "star",
Sum               = "dot",
AstroSum          = "dot",
LevelToFlow       = "diamond",
FlowToLevel       = "diamond",
DataHierarchy     = "dot"
)

nclass <- V(g)$node_class
ncol   <- unname(.class_colours[nclass])
ncol[is.na(ncol)] <- "grey80"
nshape <- unname(vis_shapes[nclass])
nshape[is.na(nshape)] <- "dot"

vis_nodes <- data.frame(
id    = V(g)$name,
label = V(g)$name,
title = paste0("<b>", V(g)$name, "</b><br>Type: ", nclass,
"<br>ID: ", V(g)$node_id),
color.background = ncol,
color.border     = "grey40",
shape = nshape,
x     = lay[, 1],
y     = lay[, 2],
stringsAsFactors = FALSE
)

# – edges data.frame ——————————————————

ecol <- unname(.edge_colours[E(g)$stroke])
ecol[is.na(ecol)] <- "grey50"
edash <- E(g)$dashed
if (is.null(edash)) edash <- rep(FALSE, ecount(g))
edash[is.na(edash)] <- FALSE

vis_edges <- data.frame(
from   = as.character(result$edges$from_name),
to     = as.character(result$edges$to_name),
color  = ecol,
dashes = edash,
title  = unname(.stroke_labels[E(g)$stroke]),
stringsAsFactors = FALSE
)
vis_edges$title[is.na(vis_edges$title)] <- ""

# – render ––––––––––––––––––––––––––––––––

visNetwork::visNetwork(vis_nodes, vis_edges, main = title,
width = "100%", height = "800px") |>
visNetwork::visEdges(arrows = "to",
smooth = list(type = "cubicBezier"),
width  = 1.5) |>
visNetwork::visNodes(font = list(size = 14)) |>
visNetwork::visPhysics(enabled = FALSE) |>
visNetwork::visOptions(
highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
nodesIdSelection = list(enabled = TRUE, main = "Select node")
) |>
visNetwork::visInteraction(navigationButtons = TRUE, keyboard = TRUE)
}

#’ Subset the network upstream or downstream of a named node
#’
#’ Uses [igraph::subcomponent()] to walk the graph in the given direction,
#’ then filters both the edge and node tables. The returned list has the
#’ same structure as [extract_svg_links()] and can be passed directly to
#’ [plot_network()] or [plot_network_interactive()].
#’
#’ @param result List returned by [extract_svg_links()].
#’ @param gauge Character. Node name to start from (e.g.
#’   `"Bedford Brackley FMP"`).
#’ @param direction Character. `"upstream"` (default) follows edges backward;
#’   `"downstream"` follows edges forward.
#’
#’ @return A list with `$edges` and `$nodes` [data.table]s.
#’
#’ @examples
#’ upstream <- subset_network(result, "Bedford Brackley FMP", "upstream")
#’ upstream$nodes
#’ plot_network_interactive(upstream, title = "Upstream of Bedford Brackley FMP")
#’
#’ @export
subset_network <- function(result, gauge, direction = "upstream") {

g <- build_graph(result)

v <- which(V(g)$name == gauge)
if (length(v) == 0) stop("No node named: ", gauge)

mode <- if (direction == "upstream") "in" else "out"
keep <- names(subcomponent(g, v, mode = mode))

list(
edges = result$edges[from_name %in% keep & to_name %in% keep],
nodes = result$nodes[node_name %in% keep]
)
}

#’ List all nodes downstream of a gauge
#’
#’ Convenience wrapper around [igraph::subcomponent()] with
#’ `mode = "out"`.
#’
#’ @param g An [igraph] object returned by [build_graph()] or
#’   [plot_network()].
#’ @param gauge Character. Node name to start from.
#’
#’ @return Character vector of downstream node names (excluding the
#’   starting gauge itself).
#’
#’ @examples
#’ g <- build_graph(result)
#’ downstream_of(g, "Brackley RG")
#’
#’ @export
downstream_of <- function(g, gauge) {
v <- which(V(g)$name == gauge)
if (length(v) == 0) stop("No node named: ", gauge)
setdiff(names(subcomponent(g, v, mode = "out")), gauge)
}

# =============================================================================
# Usage (single SVG)
# =============================================================================
# result <- extract_svg_links("network_upper_Ouse.svg")
# 
# result$edges
# result$nodes
# 
# fwrite(result$edges, "edges.csv")
# fwrite(result$nodes, "nodes.csv")
# 
# # Static Sugiyama DAG
# g <- plot_network(result, label_cex = 0.45)
# 
# # Interactive
# plot_network_interactive(result)
# 
# # Subset upstream of a gauge and plot
# upstream <- subset_network(result, "Bedford Brackley FMP", "upstream")
# upstream$nodes
# upstream$edges
# plot_network(upstream, label_cex = 0.5)
# plot_network_interactive(upstream, title = "Upstream of Bedford Brackley FMP")
# 
# # Subset downstream
# downstream <- subset_network(result, "Brackley RG", "downstream")
# plot_network_interactive(downstream, title = "Downstream of Brackley RG")
# 
# # Query downstream names
# g <- build_graph(result)
# downstream_of(g, "Brackley RG")
# downstream_of(g, "Foxcote T")
# 
# # Find all gauges feeding a process
# gauges_for(result, "Bedford Brackley FMP")
# =============================================================================
# 
# NATIONAL NETWORK LAYER
# 
# =============================================================================

#’ Extract all flood network SVGs under a national directory
#’
#’ Walks a directory structure of the form:
#’
#’ `#' root/ #'   Birmingham/ #'     Severn.svg #'     Trent.svg #'   Peterborough/ #'     Bedford_Ouse.svg #'   ... #'`
#’
#’ Each subfolder is a centre. Each SVG filename (without extension) is
#’ treated as the navtree catchment name. Calls [extract_svg_links()] on
#’ every SVG found and collates the results into three national tables.
#’
#’ @param root Character. Path to the top-level directory containing the
#’   seven centre subfolders.
#’ @param tol Numeric. Snap tolerance passed through to [extract_svg_links()].
#’   Default 5.
#’
#’ @return A list with three [data.table]s:
#’   \describe{
#’     \item{edges}{`from_name`, `to_name`, `stroke`, `dashed`, `centre`,
#’       `navtree`. One row per directed link per navtree.}
#’     \item{nodes}{`node_name`, `node_id`, `node_class`. One row per
#’       unique physical gauge/node across all SVGs.}
#’     \item{gauge_usage}{`node_name`, `node_class`, `centre`, `navtree`.
#’       Long-form: one row per gauge per navtree appearance.}
#’   }
#’
#’ @examples
#’ national <- extract_national_network("C:/path/to/networks")
#’ national$edges
#’ national$nodes
#’ national$gauge_usage
#’
#’ # Which centres use a particular rain gauge?
#’ national$gauge_usage[node_name == "Brackley RG"]
#’
#’ @export
extract_national_network <- function(root, tol = 5) {

centres <- list.dirs(root, recursive = FALSE, full.names = TRUE)

if (length(centres) == 0) stop("No centre subfolders found under: ", root)

all_edges <- list()
all_nodes <- list()
all_usage <- list()

for (centre_path in centres) {
centre <- basename(centre_path)
svgs   <- list.files(centre_path, pattern = "[.]svg$", full.names = TRUE,
                     ignore.case = TRUE)

if (length(svgs) == 0) {
  message("No SVGs in ", centre, " - skipping")
  next
}

for (svg_file in svgs) {

  navtree <- tools::file_path_sans_ext(basename(svg_file))
  message("Processing: ", centre, " / ", navtree)

  result <- tryCatch(
    extract_svg_links(svg_file, tol = tol),
    error = function(e) {
      warning("Failed: ", centre, "/", navtree, " - ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(result)) next

  # Tag edges with provenance
  edges <- copy(result$edges)
  edges[, centre := centre]
  edges[, navtree := navtree]
  all_edges[[length(all_edges) + 1]] <- edges

  # Tag nodes for usage table
  nodes <- copy(result$nodes)
  nodes[, centre := centre]
  nodes[, navtree := navtree]
  all_usage[[length(all_usage) + 1]] <- nodes
  all_nodes[[length(all_nodes) + 1]] <- result$nodes
}

}

if (length(all_edges) == 0) stop("No SVGs processed successfully.")

# National edges: keep per-navtree provenance
edges <- rbindlist(all_edges)

# National nodes: deduplicate. If the same node_name appears in multiple
# SVGs, keep the first node_id and node_class encountered (UUIDs are

# assumed consistent across SVGs).

nodes <- unique(rbindlist(all_nodes), by = "node_name")

# Gauge usage: long-form, one row per node per navtree

gauge_usage <- unique(rbindlist(all_usage)[
, .(node_name, node_class, centre, navtree)
])

n_svgs    <- nrow(unique(edges[, .(centre, navtree)]))
n_centres <- uniqueN(edges$centre)
message("Done: ", n_svgs, " SVGs across ", n_centres, " centres, ",
nrow(nodes), " unique nodes, ", nrow(edges), " edges")

list(
edges       = edges,
nodes       = nodes,
gauge_usage = gauge_usage
)
}

#’ Subset the national network to a single navtree
#’
#’ Filters the national edges and nodes tables to a single navtree,
#’ returning a result in the same format as [extract_svg_links()] so it
#’ can be passed directly to [plot_network()], [plot_network_interactive()],
#’ [subset_network()], etc.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param navtree Character. Navtree name (e.g. `"Bedford_Ouse"`).
#’ @param centre Character or NULL. Centre name to disambiguate if the same
#’   navtree name appears under multiple centres. Default NULL (matches
#’   first found).
#’
#’ @return A list with `$edges` and `$nodes` [data.table]s.
#’
#’ @examples
#’ bo <- subset_by_navtree(national, "Bedford_Ouse")
#’ plot_network_interactive(bo, title = "Bedford Ouse")
#’ subset_network(bo, "Bedford Brackley FMP", "upstream")
#’
#’ @export
subset_by_navtree <- function(national, navtree, centre = NULL) {

nav_edges <- national$edges[navtree == ..navtree]
if (!is.null(centre)) nav_edges <- nav_edges[centre == ..centre]

if (nrow(nav_edges) == 0) {
available <- unique(national$edges[, .(centre, navtree)])
stop("Navtree ‘", navtree, "’ not found.",
if (!is.null(centre)) paste0(" (centre: ‘", centre, "’)") else "",
"\nAvailable: \n",
paste(capture.output(print(available)), collapse = "\n"))
}

nav_names <- union(nav_edges$from_name, nav_edges$to_name)
nav_nodes <- national$nodes[node_name %in% nav_names]

list(
edges = nav_edges[, .(from_name, to_name, stroke, dashed)],
nodes = nav_nodes
)
}

#’ Subset the national network to a single centre
#’
#’ Filters to all navtrees belonging to a centre. Returns the same
#’ three-table national structure (with `centre` and `navtree` columns
#’ retained on edges) so it can be passed to analytics functions like
#’ [gauge_impact()], [shared_gauges()], etc.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param centre Character. Centre name (e.g. `"Peterborough"`).
#’
#’ @return A list with `$edges`, `$nodes`, and `$gauge_usage` [data.table]s,
#’   filtered to the specified centre.
#’
#’ @examples
#’ pboro <- subset_by_centre(national, "Peterborough")
#’ shared_gauges(pboro)
#’ gauge_impact(pboro, "Brackley RG")
#’
#’ @export
subset_by_centre <- function(national, centre) {

ctr_edges <- national$edges[centre == ..centre]

if (nrow(ctr_edges) == 0) {
available <- unique(national$edges$centre)
stop("Centre ‘", centre, "’ not found. Available: ",
paste(available, collapse = ", "))
}

ctr_names <- union(ctr_edges$from_name, ctr_edges$to_name)
ctr_nodes <- national$nodes[node_name %in% ctr_names]
ctr_usage <- national$gauge_usage[centre == ..centre]

list(
edges       = ctr_edges,
nodes       = ctr_nodes,
gauge_usage = ctr_usage
)
}

#’ Find all Import gauges feeding a given node
#’
#’ Walks upstream from the named node and returns only the Import (and
#’ GriddedImport) nodes that have a path to it. Convenience wrapper
#’ around [subset_network()].
#’
#’ For national data, specify `navtree` (and optionally `centre`) to
#’ scope the search to a single model. For single-SVG results, pass
#’ directly.
#’
#’ @param result List returned by [extract_svg_links()],
#’   [subset_by_navtree()], or any result with `$edges` and `$nodes`.
#’ @param node_name Character. The target node (e.g.
#’   `"Bedford Brackley FMP"`).
#’
#’ @return A [data.table] of upstream Import nodes with columns
#’   `node_name`, `node_id`, `node_class`.
#’
#’ @examples
#’ # Single SVG
#’ gauges_for(result, "Bedford Brackley FMP")
#’
#’ # From national data
#’ bo <- subset_by_navtree(national, "Bedford_Ouse")
#’ gauges_for(bo, "Bedford Brackley FMP")
#’
#’ @export
gauges_for <- function(result, node_name) {

if (!node_name %in% result$nodes$node_name) {
stop("Node ‘", node_name, "’ not found. ",
"Check result$nodes$node_name for available names.")
}

upstream <- subset_network(result, node_name, "upstream")
upstream$nodes[node_class %in% c("Import", "GriddedImport")]
}

#’ List available navtrees in the national network
#’
#’ Quick reference showing every centre/navtree combination and the
#’ number of nodes and edges in each.
#’
#’ @param national List returned by [extract_national_network()].
#’
#’ @return A [data.table] with columns `centre`, `navtree`, `n_nodes`,
#’   `n_edges`.
#’
#’ @examples
#’ list_navtrees(national)
#’ list_navtrees(national)[centre == "Peterborough"]
#’
#’ @export
list_navtrees <- function(national) {

edge_counts <- national$edges[, .(n_edges = .N), by = .(centre, navtree)]

node_counts <- national$edges[, .(
nodes = union(from_name, to_name)
), by = .(centre, navtree)][, .(n_nodes = .N), by = .(centre, navtree)]

out <- merge(edge_counts, node_counts, by = c("centre", "navtree"))
setorder(out, centre, navtree)
out
}

#’ Assess the impact of removing a gauge from the national network
#’
#’ For a given gauge, identifies every navtree that uses it, then traces
#’ the full downstream cascade within each navtree to find every forecasting
#’ point that would be affected.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param gauge Character. The gauge name to assess (e.g. `"Brackley RG"`).
#’
#’ @return A list with two [data.table]s:
#’   \describe{
#’     \item{summary}{One row per affected navtree: `centre`, `navtree`,
#’       `n_downstream`, `fmps_affected` (comma-separated names of
#’       downstream FMP nodes).}
#’     \item{detail}{Long-form: `centre`, `navtree`, `downstream_node`,
#’       `node_class`. One row per downstream-dependent node per navtree.}
#’   }
#’
#’ @examples
#’ impact <- gauge_impact(national, "Brackley RG")
#’ impact$summary
#’ impact$detail
#’
#’ @export
gauge_impact <- function(national, gauge) {

usage <- national$gauge_usage[node_name == gauge]

if (nrow(usage) == 0) {
stop("Gauge ‘", gauge, "’ not found in the national network. ",
"Check spelling against national$nodes$node_name")
}

detail_list  <- list()
summary_list <- list()

for (i in seq_len(nrow(usage))) {


ctr <- usage$centre[i]
nav <- usage$navtree[i]

# Build the subgraph for this navtree
nav_edges <- national$edges[centre == ctr & navtree == nav]
nav_nodes <- national$nodes[
  node_name %in% union(nav_edges$from_name, nav_edges$to_name)
]

nav_result <- list(edges = nav_edges, nodes = nav_nodes)
g <- build_graph(nav_result)

v <- which(V(g)$name == gauge)
if (length(v) == 0) next

ds_names <- setdiff(names(subcomponent(g, v, mode = "out")), gauge)
if (length(ds_names) == 0) next

ds_classes <- V(g)[ds_names]$node_class

detail_list[[length(detail_list) + 1]] <- data.table(
  centre          = ctr,
  navtree         = nav,
  downstream_node = ds_names,
  node_class      = ds_classes
)

fmps <- ds_names[ds_classes == "FMP"]

summary_list[[length(summary_list) + 1]] <- data.table(
  centre        = ctr,
  navtree       = nav,
  n_downstream  = length(ds_names),
  fmps_affected = if (length(fmps) > 0) paste(fmps, collapse = ", ") else ""
)


}

detail  <- if (length(detail_list) > 0) rbindlist(detail_list) else
data.table(centre = character(), navtree = character(),
downstream_node = character(), node_class = character())

summary <- if (length(summary_list) > 0) rbindlist(summary_list) else
data.table(centre = character(), navtree = character(),
n_downstream = integer(), fmps_affected = character())

message("Gauge ‘", gauge, "’ used in ", nrow(usage), " navtree(s), ",
"affecting ", sum(summary$n_downstream), " downstream node(s)")

list(summary = summary, detail = detail)
}

#’ Find all gauges shared across multiple navtrees
#’
#’ Identifies gauges that appear in more than one navtree, sorted by the
#’ number of navtrees they appear in. These are the high-impact single
#’ points of failure.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param min_navtrees Integer. Minimum number of navtrees a gauge must
#’   appear in to be included. Default 2.
#’
#’ @return A [data.table] with columns `node_name`, `node_class`,
#’   `n_navtrees`, `n_centres`, `navtrees` (comma-separated),
#’   `centres` (comma-separated), ordered by `n_navtrees` descending.
#’
#’ @examples
#’ shared_gauges(national)
#’ shared_gauges(national, min_navtrees = 3)
#’
#’ @export
shared_gauges <- function(national, min_navtrees = 2L) {

usage <- national$gauge_usage

out <- usage[, .(
node_class  = first(node_class),
n_navtrees  = uniqueN(navtree),
n_centres   = uniqueN(centre),
navtrees    = paste(unique(navtree), collapse = ", "),
centres     = paste(unique(centre), collapse = ", ")
), by = node_name]

out <- out[n_navtrees >= min_navtrees][order(-n_navtrees)]

message(nrow(out), " gauge(s) shared across >= ", min_navtrees, " navtrees")
out
}

# =============================================================================
# 
# NETWORK ANALYTICS
# 
# =============================================================================

#’ Measure forecast chain depth from Imports to FMPs
#’
#’ For each navtree, computes the number of processing steps between every
#’ Import node and every reachable FMP. Longer chains are more fragile since
#’ any single node failure breaks the forecast.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param max_depth Integer. Chains longer than this are flagged. Default 5.
#’
#’ @return A [data.table] with columns `centre`, `navtree`, `import_node`,
#’   `fmp_node`, `chain_depth`, `flagged` (logical, TRUE if
#’   `chain_depth > max_depth`), ordered by `chain_depth` descending.
#’
#’ @examples
#’ chains <- forecast_chain_depth(national)
#’ chains[flagged == TRUE]
#’
#’ @export
forecast_chain_depth <- function(national, max_depth = 5L) {

navtrees <- unique(national$edges[, .(centre, navtree)])
results  <- list()

for (i in seq_len(nrow(navtrees))) {


ctr <- navtrees$centre[i]
nav <- navtrees$navtree[i]

nav_edges <- national$edges[centre == ctr & navtree == nav]
nav_names <- union(nav_edges$from_name, nav_edges$to_name)
nav_nodes <- national$nodes[node_name %in% nav_names]

nav_result <- list(
  edges = nav_edges[, .(from_name, to_name, stroke, dashed)],
  nodes = nav_nodes
)
g <- build_graph(nav_result)

imports <- V(g)[V(g)$node_class %in% c("Import", "GriddedImport")]$name
fmps    <- V(g)[V(g)$node_class == "FMP"]$name

if (length(imports) == 0 || length(fmps) == 0) next

dists <- distances(g, v = imports, to = fmps, mode = "out")

for (ri in seq_along(imports)) {
  for (ci in seq_along(fmps)) {
    d <- dists[ri, ci]
    if (is.finite(d)) {
      results[[length(results) + 1]] <- data.table(
        centre      = ctr,
        navtree     = nav,
        import_node = imports[ri],
        fmp_node    = fmps[ci],
        chain_depth = as.integer(d)
      )
    }
  }
}


}

if (length(results) == 0) {
message("No Import-to-FMP chains found.")
return(data.table(centre = character(), navtree = character(),
import_node = character(), fmp_node = character(),
chain_depth = integer(), flagged = logical()))
}

out <- rbindlist(results)
out[, flagged := chain_depth > max_depth]
setorder(out, -chain_depth)

message(nrow(out), " chains found, ",
sum(out$flagged), " flagged (depth > ", max_depth, ")")
out
}

#’ Detect orphan or misconfigured nodes
#’
#’ Identifies:
#’ \itemize{
#’   \item Imports with no outgoing edges (feed nothing).
#’   \item FMPs with no incoming edges (receive no input).
#’   \item Non-terminal nodes with no outgoing edges that are not FMPs
#’     (dead ends).
#’   \item Non-source nodes with no incoming edges that are not Imports
#’     (disconnected).
#’ }
#’
#’ @param national List returned by [extract_national_network()].
#’
#’ @return A [data.table] with columns `centre`, `navtree`, `node_name`,
#’   `node_class`, `issue`.
#’
#’ @examples
#’ orphans <- detect_orphans(national)
#’ orphans
#’
#’ @export
detect_orphans <- function(national) {

navtrees <- unique(national$edges[, .(centre, navtree)])
results  <- list()

for (i in seq_len(nrow(navtrees))) {


ctr <- navtrees$centre[i]
nav <- navtrees$navtree[i]

nav_edges <- national$edges[centre == ctr & navtree == nav]
nav_names <- union(nav_edges$from_name, nav_edges$to_name)
nav_nodes <- national$nodes[node_name %in% nav_names]

nav_result <- list(
  edges = nav_edges[, .(from_name, to_name, stroke, dashed)],
  nodes = nav_nodes
)
g <- build_graph(nav_result)

for (v in V(g)) {
  nm  <- V(g)[v]$name
  cls <- V(g)[v]$node_class
  din  <- degree(g, v, mode = "in")
  dout <- degree(g, v, mode = "out")

  issues <- character()

  source_classes   <- c("Import", "GriddedImport")
  terminal_classes <- c("FMP", "DataHierarchy")

  if (cls %in% source_classes && dout == 0) {
    issues <- c(issues, paste0(cls, " feeds nothing"))
  }
  if (cls == "FMP" && din == 0) {
    issues <- c(issues, "FMP receives no input")
  }
  if (!cls %in% c(terminal_classes, source_classes) && dout == 0) {
    issues <- c(issues, "Dead end (non-terminal node with no outgoing edges)")
  }
  if (!cls %in% source_classes && din == 0) {
    issues <- c(issues, "Disconnected (non-source node with no incoming edges)")
  }

  for (issue in issues) {
    results[[length(results) + 1]] <- data.table(
      centre     = ctr,
      navtree    = nav,
      node_name  = nm,
      node_class = cls,
      issue      = issue
    )
  }
}


}

if (length(results) == 0) {
message("No orphan nodes detected.")
return(data.table(centre = character(), navtree = character(),
node_name = character(), node_class = character(),
issue = character()))
}

out <- rbindlist(results)
message(nrow(out), " issue(s) detected across ",
uniqueN(out$navtree), " navtree(s)")
out
}

#’ Identify gauges shared across forecast centres
#’
#’ Filters [shared_gauges()] to only those shared across different centres,
#’ not just different navtrees within the same centre. These are operationally
#’ sensitive: decommissioning by one centre can break another centre’s models.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param min_centres Integer. Minimum number of centres a gauge must span.
#’   Default 2.
#’
#’ @return A [data.table] with columns `node_name`, `node_class`,
#’   `n_centres`, `n_navtrees`, `centres` (comma-separated),
#’   `navtrees` (comma-separated), ordered by `n_centres` descending.
#’
#’ @examples
#’ cross_centre_gauges(national)
#’
#’ @export
cross_centre_gauges <- function(national, min_centres = 2L) {

usage <- national$gauge_usage

out <- usage[, .(
node_class = first(node_class),
n_centres  = uniqueN(centre),
n_navtrees = uniqueN(navtree),
centres    = paste(unique(centre), collapse = ", "),
navtrees   = paste(unique(navtree), collapse = ", ")
), by = node_name]

out <- out[n_centres >= min_centres][order(-n_centres, -n_navtrees)]

message(nrow(out), " gauge(s) shared across >= ", min_centres, " centres")
out
}

#’ Score FMP redundancy by independent import paths
#’
#’ For each FMP in each navtree, counts how many distinct Import nodes can
#’ reach it. An FMP fed by a single rain gauge through a single chain has
#’ zero redundancy. One fed by three independent gauge paths is more robust.
#’
#’ @param national List returned by [extract_national_network()].
#’
#’ @return A [data.table] with columns `centre`, `navtree`, `fmp_node`,
#’   `n_imports`, `import_names` (comma-separated), `redundancy_class`
#’   (character: "none", "low", "moderate", "high"), ordered by
#’   `n_imports` ascending.
#’
#’ @examples
#’ red <- fmp_redundancy(national)
#’ red[redundancy_class == "none"]
#’
#’ @export
fmp_redundancy <- function(national) {

navtrees <- unique(national$edges[, .(centre, navtree)])
results  <- list()

for (i in seq_len(nrow(navtrees))) {


ctr <- navtrees$centre[i]
nav <- navtrees$navtree[i]

nav_edges <- national$edges[centre == ctr & navtree == nav]
nav_names <- union(nav_edges$from_name, nav_edges$to_name)
nav_nodes <- national$nodes[node_name %in% nav_names]

nav_result <- list(
  edges = nav_edges[, .(from_name, to_name, stroke, dashed)],
  nodes = nav_nodes
)
g <- build_graph(nav_result)

imports <- V(g)[V(g)$node_class %in% c("Import", "GriddedImport")]$name
fmps    <- V(g)[V(g)$node_class == "FMP"]$name

if (length(fmps) == 0) next

for (fmp in fmps) {
  # Walk upstream from FMP and find which Imports are reachable
  fmp_v    <- which(V(g)$name == fmp)
  upstream <- names(subcomponent(g, fmp_v, mode = "in"))
  feeding  <- intersect(upstream, imports)

  n <- length(feeding)
  rc <- if (n == 0) "none" else if (n == 1) "low" else
        if (n <= 3) "moderate" else "high"

  results[[length(results) + 1]] <- data.table(
    centre           = ctr,
    navtree          = nav,
    fmp_node         = fmp,
    n_imports        = n,
    import_names     = paste(feeding, collapse = ", "),
    redundancy_class = rc
  )
}


}

if (length(results) == 0) {
message("No FMPs found.")
return(data.table(centre = character(), navtree = character(),
fmp_node = character(), n_imports = integer(),
import_names = character(), redundancy_class = character()))
}

out <- rbindlist(results)
setorder(out, n_imports)

message(nrow(out), " FMPs scored: ",
sum(out$redundancy_class == "none"), " with no imports, ",
sum(out$redundancy_class == "low"), " low, ",
sum(out$redundancy_class == "moderate"), " moderate, ",
sum(out$redundancy_class == "high"), " high redundancy")
out
}

#’ Summarise model type coverage by navtree
#’
#’ Pivots the gauge usage table to show how many nodes of each class
#’ (PDM, ARMA, FMP, CatAvg, etc.) exist per navtree. Useful for spotting
#’ navtrees that lack error correction (no ARMA), use DataHierarchy nodes
#’ (boundary imports from neighbouring models), or have unusual structures.
#’
#’ @param national List returned by [extract_national_network()].
#’
#’ @return A [data.table] in wide format: one row per centre/navtree, one
#’   column per node class, with counts. An additional `has_arma` logical
#’   column flags whether the navtree has any ARMA nodes.
#’
#’ @examples
#’ coverage <- model_type_coverage(national)
#’ coverage[has_arma == FALSE]
#’
#’ @export
model_type_coverage <- function(national) {

usage <- national$gauge_usage

# Count per navtree per class

counts <- usage[, .N, by = .(centre, navtree, node_class)]

# Pivot wide

wide <- dcast(counts, centre + navtree ~ node_class, value.var = "N",
fill = 0L)

# Flag navtrees without error correction

if ("ARMA" %in% names(wide)) {
wide[, has_arma := ARMA > 0]
} else {
wide[, has_arma := FALSE]
}

# Flag navtrees with boundary imports

if ("DataHierarchy" %in% names(wide)) {
wide[, has_boundary := DataHierarchy > 0]
} else {
wide[, has_boundary := FALSE]
}

setorder(wide, centre, navtree)

message(nrow(wide), " navtrees, ",
sum(!wide$has_arma), " without ARMA, ",
sum(wide$has_boundary), " with boundary imports")
wide
}

#’ Rank gauges by criticality
#’
#’ Computes a composite criticality score for each gauge (Import node) based
#’ on: the number of navtrees it appears in, the total number of downstream
#’ nodes across all navtrees, the number of downstream FMPs, and optionally
#’ the number and vulnerability of FWAs affected.
#’
#’ The score is a weighted sum (configurable via `weights`) normalised to
#’ 0–100. Higher = more critical.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param fwa Optional. A [data.table] returned by [load_fwa_lookup()]. If
#’   provided, FWA impact is included in the score.
#’ @param weights Named numeric vector of component weights. Defaults:
#’   `c(navtrees = 1, downstream = 1, fmps = 2, fwas = 3, critical_fwas = 5)`.
#’   Components not applicable (e.g. `fwas` when no FWA lookup is given)
#’   are ignored.
#’ @param top_n Integer or NULL. If set, return only the top N gauges.
#’   Default NULL (return all).
#’
#’ @return A [data.table] with columns `node_name`, `n_navtrees`,
#’   `n_downstream`, `n_fmps`, `fmp_names`, and optionally `n_fwas`,
#’   `n_critical_fwas`, `score`, ordered by `score` descending.
#’
#’ @examples
#’ gauge_criticality(national)
#’ gauge_criticality(national, fwa, top_n = 10)
#’
#’ @export
gauge_criticality <- function(national, fwa = NULL,
weights = c(navtrees = 1, downstream = 1,
fmps = 2, fwas = 3,
critical_fwas = 5),
top_n = NULL) {

imports <- national$nodes[node_class %in% c("Import", "GriddedImport")]$node_name
if (length(imports) == 0) {
message("No Import nodes found.")
return(data.table())
}

results <- list()

for (gauge in imports) {


usage <- national$gauge_usage[node_name == gauge]
n_nav <- nrow(usage)

# Downstream cascade across all navtrees
impact <- tryCatch(gauge_impact(national, gauge), error = function(e) NULL)

if (is.null(impact) || nrow(impact$detail) == 0) {
  n_ds   <- 0L
  n_fmps <- 0L
  fmp_nms <- ""
} else {
  n_ds    <- nrow(impact$detail)
  fmp_det <- impact$detail[node_class == "FMP"]
  n_fmps  <- nrow(fmp_det)
  fmp_nms <- paste(unique(fmp_det$downstream_node), collapse = ", ")
}

row <- data.table(
  node_name    = gauge,
  n_navtrees   = n_nav,
  n_downstream = n_ds,
  n_fmps       = n_fmps,
  fmp_names    = fmp_nms
)

# FWA component if lookup provided
if (!is.null(fwa)) {
  fi <- tryCatch(
    fwa_impact(national, fwa, gauge),
    error = function(e) NULL
  )
  if (!is.null(fi) && nrow(fi$fwa_summary) > 0) {
    row[, n_fwas := nrow(fi$fwa_summary)]
    row[, n_critical_fwas := sum(fi$fwa_summary$impact_class == "fully_impacted")]
  } else {
    row[, c("n_fwas", "n_critical_fwas") := list(0L, 0L)]
  }
}

results[[length(results) + 1]] <- row


}

out <- rbindlist(results, fill = TRUE)

# Compute composite score (normalise each component to 0-1, then weight)
normalise <- function(x) {
r <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
if (r == 0) rep(0, length(x)) else (x - min(x, na.rm = TRUE)) / r
}

score <- weights["navtrees"] * normalise(out$n_navtrees) +
weights["downstream"] * normalise(out$n_downstream) +
weights["fmps"] * normalise(out$n_fmps)

if ("n_fwas" %in% names(out)) {
score <- score +
weights["fwas"] * normalise(out$n_fwas) +
weights["critical_fwas"] * normalise(out$n_critical_fwas)
}

# Scale to 0-100
sr <- max(score, na.rm = TRUE) - min(score, na.rm = TRUE)
out[, score := if (sr == 0) 0 else round(100 * (score - min(score)) / sr, 1)]

setorder(out, -score)

if (!is.null(top_n)) out <- head(out, top_n)

message(nrow(out), " Import gauges scored")
out
}

#’ Compute maximum cascade distance per Import gauge
#’
#’ For each Import node, finds the length of the longest directed path to
#’ any downstream FMP. This measures the maximum blast radius of a single
#’ gauge failure — a gauge with cascade distance 8 does far more structural
#’ damage than one with distance 2.
#’
#’ @param national List returned by [extract_national_network()].
#’
#’ @return A [data.table] with columns `centre`, `navtree`, `import_node`,
#’   `max_distance`, `furthest_fmp`, ordered by `max_distance` descending.
#’
#’ @examples
#’ cd <- cascade_distance(national)
#’ cd[max_distance >= 6]
#’
#’ @export
cascade_distance <- function(national) {

navtrees <- unique(national$edges[, .(centre, navtree)])
results  <- list()

for (i in seq_len(nrow(navtrees))) {
ctr <- navtrees$centre[i]
nav <- navtrees$navtree[i]

nav_edges <- national$edges[centre == ctr & navtree == nav]
nav_names <- union(nav_edges$from_name, nav_edges$to_name)
nav_nodes <- national$nodes[node_name %in% nav_names]

nav_result <- list(
  edges = nav_edges[, .(from_name, to_name, stroke, dashed)],
  nodes = nav_nodes
)
g <- build_graph(nav_result)

imports <- V(g)[V(g)$node_class %in% c("Import", "GriddedImport")]$name
fmps    <- V(g)[V(g)$node_class == "FMP"]$name

if (length(imports) == 0 || length(fmps) == 0) next

dists <- distances(g, v = imports, to = fmps, mode = "out")

for (ri in seq_along(imports)) {
  row_dists <- dists[ri, ]
  finite    <- is.finite(row_dists)
  if (!any(finite)) next

  max_d    <- max(row_dists[finite])
  furthest <- fmps[finite][which.max(row_dists[finite])]

  results[[length(results) + 1]] <- data.table(
    centre       = ctr,
    navtree      = nav,
    import_node  = imports[ri],
    max_distance = as.integer(max_d),
    furthest_fmp = furthest
  )
}

}

if (length(results) == 0) {
message("No Import-to-FMP paths found.")
return(data.table(centre = character(), navtree = character(),
import_node = character(), max_distance = integer(),
furthest_fmp = character()))
}

out <- rbindlist(results)
setorder(out, -max_distance)
message(nrow(out), " Import-FMP paths scored")
out
}

#’ Compare two versions of a navtree SVG
#’
#’ Extracts both SVGs independently, then reports added/removed/changed
#’ nodes and edges. Useful for configuration management when a navtree
#’ is updated between FEWS releases.
#’
#’ @param svg_old Character. Path to the old SVG.
#’ @param svg_new Character. Path to the new SVG.
#’ @param tol Numeric. Snap tolerance passed to [extract_svg_links()].
#’   Default 5.
#’
#’ @return A list with four [data.table]s:
#’   \describe{
#’     \item{nodes_added}{Nodes in new but not old.}
#’     \item{nodes_removed}{Nodes in old but not new.}
#’     \item{edges_added}{Edges in new but not old (by from/to name).}
#’     \item{edges_removed}{Edges in old but not new (by from/to name).}
#’   }
#’
#’ @examples
#’ diff <- network_diff("Severn_v1.svg", "Severn_v2.svg")
#’ diff$nodes_added
#’ diff$edges_removed
#’
#’ @export
network_diff <- function(svg_old, svg_new, tol = 5) {

old <- extract_svg_links(svg_old, tol = tol)
new <- extract_svg_links(svg_new, tol = tol)

# Node diff

nodes_added   <- new$nodes[!node_name %in% old$nodes$node_name]
nodes_removed <- old$nodes[!node_name %in% new$nodes$node_name]

# Edge diff (keyed on from_name + to_name)

old_key <- old$edges[, .(from_name, to_name)]
new_key <- new$edges[, .(from_name, to_name)]

old_key[, key := paste(from_name, to_name, sep = "->")]
new_key[, key := paste(from_name, to_name, sep = "->")]

edges_added   <- new$edges[paste(from_name, to_name, sep = "->") %in%
setdiff(new_key$key, old_key$key)]
edges_removed <- old$edges[paste(from_name, to_name, sep = "->") %in%
setdiff(old_key$key, new_key$key)]

message("Nodes: +", nrow(nodes_added), " / -", nrow(nodes_removed),
"  Edges: +", nrow(edges_added), " / -", nrow(edges_removed))

list(
nodes_added   = nodes_added,
nodes_removed = nodes_removed,
edges_added   = edges_added,
edges_removed = edges_removed
)
}

#’ Generate a national summary report
#’
#’ Runs all analytics and writes a self-contained markdown report. One
#’ section per centre plus a national overview. Optionally includes FWA
#’ analysis if a lookup is provided.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param fwa Optional. A [data.table] returned by [load_fwa_lookup()].
#’ @param output_file Character. Path for the output markdown file.
#’   Default `"national_report.md"`.
#’ @param top_n_critical Integer. Number of top critical gauges to show.
#’   Default 20.
#’
#’ @return The output file path, invisibly.
#’
#’ @examples
#’ national_summary_report(national)
#’ national_summary_report(national, fwa, output_file = "report.md")
#’
#’ @export
national_summary_report <- function(national, fwa = NULL,
output_file = "national_report.md",
top_n_critical = 20L) {

lines <- character()
add   <- function(…) lines <<- c(lines, paste0(…))
blank <- function() lines <<- c(lines, "")
table_md <- function(dt) {
if (nrow(dt) == 0) { add("*No data.*"); blank(); return(invisible()) }
nms <- names(dt)
add("| ", paste(nms, collapse = " | "), " |")
add("| ", paste(rep("—", length(nms)), collapse = " | "), " |")
for (r in seq_len(nrow(dt))) {
vals <- vapply(nms, function(n) as.character(dt[[n]][r]), character(1))
add("| ", paste(vals, collapse = " | "), " |")
}
blank()
}

timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M")

# – Header —————————————————————
add("# National Flood Forecast Network Report")
add("*Generated: ", timestamp, "*")
blank()

# – Overview –––––––––––––––––––––––––––––––
add("## Overview")
blank()

n_centres  <- uniqueN(national$gauge_usage$centre)
n_navtrees <- uniqueN(national$gauge_usage[, paste(centre, navtree)])
n_nodes    <- nrow(national$nodes)
n_edges    <- nrow(national$edges)
n_imports  <- sum(national$nodes$node_class %in% c("Import", "GriddedImport"))
n_fmps     <- sum(national$nodes$node_class == "FMP")

add("- **Centres:** ", n_centres)
add("- **Navtrees:** ", n_navtrees)
add("- **Unique nodes:** ", n_nodes, " (", n_imports, " Imports, ", n_fmps, " FMPs)")
add("- **Edges:** ", n_edges)
blank()

# – Model type coverage —————————————————
add("## Model Type Coverage")
blank()

coverage <- model_type_coverage(national)
table_md(coverage)

arma_missing <- coverage[has_arma == FALSE]
if (nrow(arma_missing) > 0) {
add("**Warning:** ", nrow(arma_missing), " navtree(s) without ARMA error correction:")
for (r in seq_len(nrow(arma_missing))) {
add("- ", arma_missing$centre[r], " / ", arma_missing$navtree[r])
}
blank()
}

# – Shared gauges ———————————————————
add("## Shared Gauges")
blank()

shared <- shared_gauges(national)
if (nrow(shared) > 0) {
table_md(shared[, .(node_name, node_class, n_navtrees, n_centres, centres)])
} else {
add("No gauges shared across multiple navtrees.")
blank()
}

# – Cross-centre gauges —————————————————
add("## Cross-Centre Dependencies")
blank()

xcentre <- cross_centre_gauges(national)
if (nrow(xcentre) > 0) {
table_md(xcentre[, .(node_name, node_class, n_centres, centres)])
} else {
add("No gauges shared across centres.")
blank()
}

# – Gauge criticality —————————————————–
add("## Top ", top_n_critical, " Critical Gauges")
blank()

crit <- gauge_criticality(national, fwa = fwa, top_n = top_n_critical)
show_cols <- c("node_name", "score", "n_navtrees", "n_downstream", "n_fmps")
if ("n_fwas" %in% names(crit)) {
show_cols <- c(show_cols, "n_fwas", "n_critical_fwas")
}
table_md(crit[, ..show_cols])

# – Cascade distance ——————————————————
add("## Longest Cascade Distances")
blank()

cd <- cascade_distance(national)
if (nrow(cd) > 0) {
table_md(head(cd, 20))
} else {
add("No Import-to-FMP paths found.")
blank()
}

# – Orphans —————————————————————
add("## Orphan / Misconfigured Nodes")
blank()

orphans <- detect_orphans(national)
if (nrow(orphans) > 0) {
table_md(orphans)
} else {
add("No orphan nodes detected.")
blank()
}

# – FMP redundancy —————————————————––
add("## FMP Redundancy")
blank()

red <- fmp_redundancy(national)
vulnerable <- red[redundancy_class %in% c("none", "low")]
if (nrow(vulnerable) > 0) {
add("FMPs with no or low import redundancy:")
blank()
table_md(vulnerable[, .(centre, navtree, fmp_node, n_imports,
redundancy_class, import_names)])
} else {
add("All FMPs have moderate or high redundancy.")
blank()
}

# – FWA section (optional) ————————————————
if (!is.null(fwa)) {
add("## Flood Warning Area Resilience")
blank()


res <- fwa_resilience(fwa)
add("### Resilience Summary")
blank()
add("- **Robust:** ", sum(res$resilience_class == "robust"))
add("- **Forecast vulnerable:** ", sum(res$resilience_class == "forecast_vulnerable"))
add("- **Reactive vulnerable:** ", sum(res$resilience_class == "reactive_vulnerable"))
add("- **Critical:** ", sum(res$resilience_class == "critical"))
blank()

critical_fwas <- res[resilience_class == "critical"]
if (nrow(critical_fwas) > 0) {
  add("### Critical FWAs")
  blank()
  table_md(critical_fwas[, .(fwa_code, n_forecast, forecast_nodes,
                             n_observed, observed_nodes)])
}


}

# – Per-centre summaries –––––––––––––––––––––––––
add("## Centre Summaries")
blank()

centres <- sort(unique(national$gauge_usage$centre))
for (ctr in centres) {
add("### ", ctr)
blank()


ctr_navtrees <- unique(national$gauge_usage[centre == ctr]$navtree)
ctr_nodes    <- national$gauge_usage[centre == ctr, uniqueN(node_name)]
ctr_edges    <- national$edges[centre == ctr, .N]

add("- **Navtrees:** ", paste(ctr_navtrees, collapse = ", "))
add("- **Nodes:** ", ctr_nodes)
add("- **Edges:** ", ctr_edges)
blank()

# Orphans in this centre
ctr_orphans <- orphans[centre == ctr]
if (nrow(ctr_orphans) > 0) {
  add("**Issues:** ", nrow(ctr_orphans), " orphan/misconfigured node(s)")
  blank()
}


}

# – Write —————————————————————–

writeLines(lines, output_file)
message("Report written to: ", output_file)
invisible(output_file)
}

# =============================================================================
# 
# FLOOD WARNING AREA LAYER
# 
# =============================================================================

#’ Load a Flood Warning Area lookup table
#’
#’ Reads a CSV mapping node names to FWA codes with an association type.
#’ The CSV must have three columns:
#’ \describe{
#’   \item{node_name}{Character. Must match a name in the national network
#’     (e.g. `"Bedford Tove FMP"`, `"Cappenham"`).}
#’   \item{fwa_code}{Character. The Flood Warning Area code
#’     (e.g. `"034FWFTOVE"`).}
#’   \item{association}{Character. Either `"forecast"` (node provides model
#’     output for this FWA) or `"observed"` (node provides reactive observed
#’     data for this FWA).}
#’ }
#’
#’ Validates that all node names exist in the national network and warns
#’ about any that do not match.
#’
#’ @param csv_file Character. Path to the CSV file.
#’ @param national List returned by [extract_national_network()]. Used for
#’   validation only.
#’
#’ @return A [data.table] with columns `node_name`, `fwa_code`, `association`.
#’
#’ @examples
#’ fwa <- load_fwa_lookup("fwa_lookup.csv", national)
#’
#’ @export
load_fwa_lookup <- function(csv_file, national = NULL) {

fwa <- fread(csv_file)

required <- c("node_name", "fwa_code", "association")
missing  <- setdiff(required, names(fwa))
if (length(missing) > 0) {
stop("Missing columns: ", paste(missing, collapse = ", "),
". Required: node_name, fwa_code, association")
}

fwa <- fwa[, ..required]
fwa[, association := tolower(trimws(association))]

bad_assoc <- setdiff(unique(fwa$association), c("forecast", "observed"))
if (length(bad_assoc) > 0) {
stop("Invalid association values: ", paste(bad_assoc, collapse = ", "),
". Must be ‘forecast’ or ‘observed’.")
}

if (!is.null(national)) {
unknown <- setdiff(fwa$node_name, national$nodes$node_name)
if (length(unknown) > 0) {
warning(length(unknown), " node name(s) in lookup not found in national ",
"network: ", paste(head(unknown, 10), collapse = ", "),
if (length(unknown) > 10) "…" else "")
}
}

n_fwa <- uniqueN(fwa$fwa_code)
n_fc  <- sum(fwa$association == "forecast")
n_ob  <- sum(fwa$association == "observed")
message("Loaded: ", n_fwa, " FWAs, ", n_fc, " forecast links, ",
n_ob, " observed links")

fwa
}

#’ Score baseline resilience of each Flood Warning Area
#’
#’ For each FWA, counts the number of forecast sources (FMP/PDM nodes)
#’ and observed sources (Import/gauge nodes) that serve it. FWAs with
#’ low counts on either axis are more vulnerable to outages.
#’
#’ @param fwa A [data.table] returned by [load_fwa_lookup()].
#’
#’ @return A [data.table] with columns `fwa_code`, `n_forecast`,
#’   `n_observed`, `forecast_nodes` (comma-separated),
#’   `observed_nodes` (comma-separated), `resilience_class` (character:
#’   `"robust"`, `"forecast_vulnerable"`, `"reactive_vulnerable"`,
#’   `"critical"`).
#’
#’ @details Resilience classification:
#’ \describe{
#’   \item{robust}{Multiple forecast and multiple observed sources.}
#’   \item{forecast_vulnerable}{Single forecast source but multiple observed.}
#’   \item{reactive_vulnerable}{Multiple forecast but single observed source.}
#’   \item{critical}{Single (or zero) sources on both axes.}
#’ }
#’
#’ @examples
#’ res <- fwa_resilience(fwa)
#’ res[resilience_class == "critical"]
#’
#’ @export
fwa_resilience <- function(fwa) {

fc <- fwa[association == "forecast", .(
n_forecast     = .N,
forecast_nodes = paste(node_name, collapse = ", ")
), by = fwa_code]

ob <- fwa[association == "observed", .(
n_observed     = .N,
observed_nodes = paste(node_name, collapse = ", ")
), by = fwa_code]

all_fwas <- data.table(fwa_code = unique(fwa$fwa_code))
out <- merge(all_fwas, fc, by = "fwa_code", all.x = TRUE)
out <- merge(out, ob, by = "fwa_code", all.x = TRUE)

out[is.na(n_forecast), c("n_forecast", "forecast_nodes") := list(0L, "")]
out[is.na(n_observed), c("n_observed", "observed_nodes") := list(0L, "")]

out[, resilience_class := fcase(
n_forecast >= 2 & n_observed >= 2, "robust",
n_forecast <= 1 & n_observed >= 2, "forecast_vulnerable",
n_forecast >= 2 & n_observed <= 1, "reactive_vulnerable",
default = "critical"
)]

setorder(out, resilience_class, fwa_code)

message(nrow(out), " FWAs: ",
sum(out$resilience_class == "critical"), " critical, ",
sum(out$resilience_class == "forecast_vulnerable"), " forecast-vulnerable, ",
sum(out$resilience_class == "reactive_vulnerable"), " reactive-vulnerable, ",
sum(out$resilience_class == "robust"), " robust")
out
}

#’ Assess FWA impact of a gauge outage
#’
#’ Given a failed gauge, traces the downstream cascade through the national
#’ network, then cross-references with the FWA lookup to classify the
#’ impact on each affected Flood Warning Area.
#’
#’ Impact classifications:
#’ \describe{
#’   \item{fully_impacted}{All forecast and all observed sources for this
#’     FWA are lost.}
#’   \item{forecast_lost}{All forecast sources lost, but at least one
#’     observed source remains for reactive issuing.}
#’   \item{forecast_degraded}{At least one forecast source lost but others
#’     remain.}
#’   \item{reactive_degraded}{At least one observed source lost but others
#’     remain. Forecasts unaffected.}
#’ }
#’
#’ @param national List returned by [extract_national_network()].
#’ @param fwa A [data.table] returned by [load_fwa_lookup()].
#’ @param gauge Character. The name of the failed gauge.
#’
#’ @return A list with two [data.table]s:
#’   \describe{
#’     \item{fwa_summary}{One row per affected FWA: `fwa_code`,
#’       `impact_class`, `forecast_total`, `forecast_lost`,
#’       `observed_total`, `observed_lost`.}
#’     \item{node_detail}{One row per affected node-FWA pair: `fwa_code`,
#’       `node_name`, `association`, `status` (`"lost"` or `"ok"`).}
#’   }
#’
#’ @examples
#’ impact <- fwa_impact(national, fwa, "Brackley RG")
#’ impact$fwa_summary
#’ impact$fwa_summary[impact_class == "fully_impacted"]
#’ impact$node_detail
#’
#’ @export
fwa_impact <- function(national, fwa, gauge) {

# 1. Find all nodes knocked out by this gauge failure

# The gauge itself + everything downstream of it in every navtree

impact <- gauge_impact(national, gauge)
affected_nodes <- union(gauge, impact$detail$downstream_node)

# 2. Find all FWAs linked to any affected node

fwa_affected <- fwa[node_name %in% affected_nodes]

if (nrow(fwa_affected) == 0) {
message("No FWAs directly affected by ‘", gauge, "’")
return(list(
fwa_summary = data.table(fwa_code = character(), impact_class = character(),
forecast_total = integer(), forecast_lost = integer(),
observed_total = integer(), observed_lost = integer()),
node_detail = data.table(fwa_code = character(), node_name = character(),
association = character(), status = character())
))
}

affected_fwa_codes <- unique(fwa_affected$fwa_code)

# 3. For each affected FWA, classify impact

all_links <- fwa[fwa_code %in% affected_fwa_codes]
all_links[, status := fifelse(node_name %in% affected_nodes, "lost", "ok")]

summary_list <- list()
for (code in affected_fwa_codes) {
links <- all_links[fwa_code == code]


fc_total <- sum(links$association == "forecast")
fc_lost  <- sum(links$association == "forecast" & links$status == "lost")
ob_total <- sum(links$association == "observed")
ob_lost  <- sum(links$association == "observed" & links$status == "lost")

fc_remaining <- fc_total - fc_lost
ob_remaining <- ob_total - ob_lost

impact_class <- if (fc_remaining == 0 && ob_remaining == 0) {
  "fully_impacted"
} else if (fc_remaining == 0 && ob_remaining > 0) {
  "forecast_lost"
} else if (fc_lost > 0 && fc_remaining > 0) {
  "forecast_degraded"
} else {
  "reactive_degraded"
}

summary_list[[length(summary_list) + 1]] <- data.table(
  fwa_code       = code,
  impact_class   = impact_class,
  forecast_total = fc_total,
  forecast_lost  = fc_lost,
  observed_total = ob_total,
  observed_lost  = ob_lost
)


}

fwa_summary <- rbindlist(summary_list)

# Order by severity

severity_order <- c("fully_impacted", "forecast_lost",
"forecast_degraded", "reactive_degraded")
fwa_summary[, impact_class := factor(impact_class, levels = severity_order)]
setorder(fwa_summary, impact_class)
fwa_summary[, impact_class := as.character(impact_class)]

node_detail <- all_links[, .(fwa_code, node_name, association, status)]

message("Gauge ‘", gauge, "’: ",
sum(fwa_summary$impact_class == "fully_impacted"), " FWA(s) fully impacted, ",
sum(fwa_summary$impact_class == "forecast_lost"), " forecast lost, ",
sum(fwa_summary$impact_class == "forecast_degraded"), " forecast degraded, ",
sum(fwa_summary$impact_class == "reactive_degraded"), " reactive degraded")

list(fwa_summary = fwa_summary, node_detail = node_detail)
}

#’ Assess FWA impact of multiple simultaneous gauge outages
#’
#’ Extension of [fwa_impact()] for scenarios where several gauges fail at
#’ once (e.g. a telemetry outstation going down, or a regional power
#’ failure). Unions the downstream cascades of all failed gauges before
#’ classifying FWA impact.
#’
#’ @param national List returned by [extract_national_network()].
#’ @param fwa A [data.table] returned by [load_fwa_lookup()].
#’ @param gauges Character vector. Names of all failed gauges.
#’
#’ @return Same structure as [fwa_impact()]: a list with `$fwa_summary`
#’   and `$node_detail`.
#’
#’ @examples
#’ # Scenario: telemetry outstation serves two gauges
#’ fwa_impact_multi(national, fwa, c("Brackley RG", "Foxcote T"))
#’
#’ @export
fwa_impact_multi <- function(national, fwa, gauges) {

all_affected <- character()

for (gauge in gauges) {
if (!gauge %in% national$nodes$node_name) {
warning("Gauge ‘", gauge, "’ not found - skipping")
next
}
impact <- gauge_impact(national, gauge)
all_affected <- union(all_affected,
union(gauge, impact$detail$downstream_node))
}

if (length(all_affected) == 0) {
message("No nodes affected by these gauges")
return(list(
fwa_summary = data.table(fwa_code = character(), impact_class = character(),
forecast_total = integer(), forecast_lost = integer(),
observed_total = integer(), observed_lost = integer()),
node_detail = data.table(fwa_code = character(), node_name = character(),
association = character(), status = character())
))
}

fwa_affected <- fwa[node_name %in% all_affected]
if (nrow(fwa_affected) == 0) {
message("No FWAs directly affected")
return(list(
fwa_summary = data.table(fwa_code = character(), impact_class = character(),
forecast_total = integer(), forecast_lost = integer(),
observed_total = integer(), observed_lost = integer()),
node_detail = data.table(fwa_code = character(), node_name = character(),
association = character(), status = character())
))
}

affected_fwa_codes <- unique(fwa_affected$fwa_code)
all_links <- fwa[fwa_code %in% affected_fwa_codes]
all_links[, status := fifelse(node_name %in% all_affected, "lost", "ok")]

summary_list <- list()
for (code in affected_fwa_codes) {
links <- all_links[fwa_code == code]


fc_total <- sum(links$association == "forecast")
fc_lost  <- sum(links$association == "forecast" & links$status == "lost")
ob_total <- sum(links$association == "observed")
ob_lost  <- sum(links$association == "observed" & links$status == "lost")

fc_remaining <- fc_total - fc_lost
ob_remaining <- ob_total - ob_lost

impact_class <- if (fc_remaining == 0 && ob_remaining == 0) {
  "fully_impacted"
} else if (fc_remaining == 0 && ob_remaining > 0) {
  "forecast_lost"
} else if (fc_lost > 0 && fc_remaining > 0) {
  "forecast_degraded"
} else {
  "reactive_degraded"
}

summary_list[[length(summary_list) + 1]] <- data.table(
  fwa_code       = code,
  impact_class   = impact_class,
  forecast_total = fc_total,
  forecast_lost  = fc_lost,
  observed_total = ob_total,
  observed_lost  = ob_lost
)


}

fwa_summary <- rbindlist(summary_list)
severity_order <- c("fully_impacted", "forecast_lost",
"forecast_degraded", "reactive_degraded")
fwa_summary[, impact_class := factor(impact_class, levels = severity_order)]
setorder(fwa_summary, impact_class)
fwa_summary[, impact_class := as.character(impact_class)]

node_detail <- all_links[, .(fwa_code, node_name, association, status)]

message("Gauges: ", paste(gauges, collapse = ", "), " — ",
sum(fwa_summary$impact_class == "fully_impacted"), " FWA(s) fully impacted, ",
sum(fwa_summary$impact_class == "forecast_lost"), " forecast lost, ",
sum(fwa_summary$impact_class == "forecast_degraded"), " forecast degraded, ",
sum(fwa_summary$impact_class == "reactive_degraded"), " reactive degraded")

list(fwa_summary = fwa_summary, node_detail = node_detail)
}

# =============================================================================
# Usage (national)
# =============================================================================
# national <- extract_national_network("C:/path/to/networks")
# 
# # Three output tables
# national$edges
# national$nodes
# national$gauge_usage
# 
# # Persist
# fwrite(national$edges, "national_edges.csv")
# fwrite(national$nodes, "national_nodes.csv")
# fwrite(national$gauge_usage, "national_gauge_usage.csv")
# 
# # Which navtrees use a particular gauge?
# national$gauge_usage[node_name == "Brackley RG"]
# 
# # List all available navtrees
# list_navtrees(national)
# 
# # — Subsetting —
# 
# # Subset to a single navtree (works with all single-SVG functions)
# bo <- subset_by_navtree(national, "Bedford_Ouse")
# plot_network_interactive(bo, title = "Bedford Ouse")
# subset_network(bo, "Bedford Brackley FMP", "upstream")
# 
# # Subset to an entire centre (works with all national functions)
# pboro <- subset_by_centre(national, "Peterborough")
# shared_gauges(pboro)
# gauge_impact(pboro, "Brackley RG")
# 
# # Find all gauges feeding a specific process
# bo <- subset_by_navtree(national, "Bedford_Ouse")
# gauges_for(bo, "Bedford Brackley FMP")
# 
# # High-impact shared gauges (single points of failure)
# shared_gauges(national)
# 
# # Full impact assessment for a gauge
# impact <- gauge_impact(national, "Brackley RG")
# impact$summary   # one row per affected navtree, with FMP names
# impact$detail    # every downstream node in every affected navtree
# 
# # Forecast chain depth - flag fragile long chains
# chains <- forecast_chain_depth(national)
# chains[flagged == TRUE]
# 
# # Orphan detection - misconfigured nodes
# orphans <- detect_orphans(national)
# orphans
# 
# # Cross-centre dependencies
# cross_centre_gauges(national)
# 
# # FMP redundancy scoring
# red <- fmp_redundancy(national)
# red[redundancy_class == "none"]    # FMPs with no feeding imports
# red[redundancy_class == "low"]     # FMPs relying on a single gauge
# 
# # Model type coverage
# coverage <- model_type_coverage(national)
# coverage[has_arma == FALSE]        # navtrees without error correction
# coverage[has_boundary == TRUE]     # navtrees with boundary imports
# 
# # Gauge criticality ranking
# crit <- gauge_criticality(national, top_n = 10)
# crit                                # top 10 most critical gauges
# gauge_criticality(national, fwa)    # includes FWA weighting if available
# 
# # Maximum cascade distance per gauge
# cd <- cascade_distance(national)
# cd[max_distance >= 6]               # gauges with long blast radius
# 
# # Compare two versions of a navtree SVG
# diff <- network_diff("Severn_v1.svg", "Severn_v2.svg")
# diff$nodes_added
# diff$edges_removed
# 
# # Generate a national summary report (markdown)
# national_summary_report(national)
# national_summary_report(national, fwa, output_file = "report.md")
# 
# # — FWA layer (opt-in) —
# 
# # Load the FWA lookup
# fwa <- load_fwa_lookup("fwa_lookup.csv", national)
# 
# # Baseline resilience of each FWA
# res <- fwa_resilience(fwa)
# res[resilience_class == "critical"]
# 
# # What happens to FWAs if a gauge goes down?
# fi <- fwa_impact(national, fwa, "Brackley RG")
# fi$fwa_summary
# fi$fwa_summary[impact_class == "fully_impacted"]
# fi$node_detail
# 
# # Multi-gauge failure scenario
# fi_multi <- fwa_impact_multi(national, fwa, c("Brackley RG", "Foxcote T"))
# fi_multi$fwa_summary
# 
# # Plot a single navtree from the national dataset
# bo <- subset_by_navtree(national, "Bedford_Ouse")
# plot_network_interactive(bo, title = "Bedford Ouse")
