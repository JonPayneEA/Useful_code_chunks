# =============================================================================
# extract_svg_links.R
#
# Extract directed links and node metadata from a FEWS-style flood-network SVG.
#
# Portability:
#   - Regex uses R 4.0+ raw strings r"(...)".
#   - XPath uses intToUtf8(39L) to avoid smart-quote mangling on copy/paste.
# =============================================================================

library(xml2)
library(data.table)
library(igraph)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a


# -- Shared colour/shape lookups used by multiple functions -------------------

.class_colours <- c(
  Import        = "#d4351c",
  CatAvg        = "#fbc9ec",
  PDM           = "#92d050",
  ARMA          = "#b7dee8",
  FMP           = "#6599d9",
  Sum           = "#fcddc4",
  DataHierarchy = "#a6a6a6"
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


#' Extract directed links from a FEWS-style flood-network SVG
#'
#' Parses the SVG's `<g id="nodes">` and `<g id="edges">` groups, matching
#' edge endpoints to node connection ports by Euclidean distance. Returns
#' human-readable node names, not UUIDs.
#'
#' @param svg_file Character. Path to the SVG file.
#' @param tol Numeric. Maximum snap distance (in SVG user units) before an
#'   edge endpoint is flagged as suspect. Default 5.
#'
#' @return A list with two [data.table]s:
#'   \describe{
#'     \item{edges}{`from_name`, `to_name`, `stroke` (RGB string), `dashed`
#'       (logical). One row per unique directed link.}
#'     \item{nodes}{`node_name`, `node_id` (shared UUID prefix for Imports),
#'       `node_class` (e.g. PDM, FMP, CatAvg, ARMA, Import, Sum,
#'       DataHierarchy).}
#'   }
#'
#' @examples
#' result <- extract_svg_links("network_upper_Ouse.svg")
#' result$edges
#' result$nodes
#'
#' @export
extract_svg_links <- function(svg_file, tol = 5) {
  
  doc <- read_xml(svg_file)
  ns  <- c(s = "http://www.w3.org/2000/svg")
  
  q        <- intToUtf8(39L)
  xp_nodes <- paste0("//s:g[contains(@class,", q, "node", q, ")]")
  xp_edges <- paste0("//s:g[@id=", q, "edges", q, "]/s:g")
  
  # -- helpers ---------------------------------------------------------------
  
  parse_translate <- function(s) {
    if (is.na(s)) return(c(0, 0))
    m <- regmatches(s, regexec(r"(translate\(([-0-9.]+)[ ,]+([-0-9.]+)\))", s))[[1]]
    if (length(m) < 3) c(0, 0) else as.numeric(m[2:3])
  }
  
  node_label <- function(g) {
    tspans <- xml_find_all(g, ".//s:text/s:tspan", ns)
    if (length(tspans) == 0) return(NA_character_)
    trimws(xml_text(tspans[[length(tspans)]]))
  }
  
  strip_node_prefix <- function(cls) sub("^node\\s+", "", cls)
  shared_id         <- function(id)  sub("/.*$", "", id)
  
  # -- 1. Ports (one row per connection point) -------------------------------
  
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
  
  # -- 2. Parse edge endpoints -----------------------------------------------
  
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
  
  # -- 3. Match endpoints to nearest port ------------------------------------
  
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
  
  # -- 4. Build outputs ------------------------------------------------------
  
  edges <- unique(raw_edges[, .(from_name, to_name, stroke, dashed)])
  
  nodes <- unique(ports[, .(
    node_id    = shared_id(first(node_id)),
    node_class = first(node_class)
  ), by = node_name])
  
  list(edges = edges, nodes = nodes)
}


#' Build an igraph object from extraction results
#'
#' Shared helper used by the plot functions and [subset_network()]. Constructs
#' a directed graph with vertex attributes `node_id`, `node_class` and edge
#' attributes `stroke`, `dashed`.
#'
#' @param result List returned by [extract_svg_links()].
#'
#' @return An [igraph] directed graph.
#'
#' @export
build_graph <- function(result) {
  edges_df <- as.data.frame(result$edges)
  names(edges_df)[1:2] <- c("from", "to")
  
  graph_from_data_frame(
    d        = edges_df,
    vertices = as.data.frame(result$nodes[, c("node_name", "node_id", "node_class")]),
    directed = TRUE
  )
}


#' Plot the network as a static Sugiyama DAG
#'
#' Uses [igraph::layout_with_sugiyama()] for a clean layered layout. Nodes
#' are coloured by class; edges are coloured by gauge type (rainfall, flow,
#' level).
#'
#' @param result List returned by [extract_svg_links()] or [subset_network()].
#' @param label_cex Numeric. Character expansion factor for vertex labels.
#'   Default 0.5.
#'
#' @return The [igraph] object, invisibly.
#'
#' @examples
#' g <- plot_network(result, label_cex = 0.45)
#'
#' @export
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


#' Plot the network as an interactive visNetwork widget
#'
#' Builds an [igraph] object, sets visual attributes for nodes and edges,
#' and converts to a [visNetwork] widget via [visNetwork::visIgraph()].
#' Layout is computed in R using Sugiyama to avoid vis.js hierarchical
#' layout issues with cyclic graphs.
#'
#' Hover over nodes to see name, type, and UUID. Click to highlight
#' neighbours. Use the dropdown to jump to a node by name.
#'
#' @param result List returned by [extract_svg_links()] or [subset_network()].
#' @param title Character. Title shown above the widget. Default
#'   "Flood Forecast Network".
#'
#' @return A [visNetwork::visNetwork] htmlwidget.
#'
#' @examples
#' plot_network_interactive(result)
#' plot_network_interactive(upstream, title = "Upstream of Bedford Brackley FMP")
#'
#' @export
plot_network_interactive <- function(result, title = "Flood Forecast Network") {
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Install visNetwork first: install.packages('visNetwork')")
  }
  
  g <- build_graph(result)
  
  # -- compute layout in R ---------------------------------------------------
  
  lay <- layout_with_sugiyama(g)$layout
  lay[, 1] <- lay[, 1] * 200
  lay[, 2] <- lay[, 2] * -150
  
  # -- nodes data.frame ------------------------------------------------------
  
  vis_shapes <- c(
    Import        = "triangle",
    CatAvg        = "dot",
    PDM           = "square",
    ARMA          = "diamond",
    FMP           = "star",
    Sum           = "dot",
    DataHierarchy = "dot"
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
  
  # -- edges data.frame ------------------------------------------------------
  
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
  
  # -- render ----------------------------------------------------------------
  
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


#' Subset the network upstream or downstream of a named node
#'
#' Uses [igraph::subcomponent()] to walk the graph in the given direction,
#' then filters both the edge and node tables. The returned list has the
#' same structure as [extract_svg_links()] and can be passed directly to
#' [plot_network()] or [plot_network_interactive()].
#'
#' @param result List returned by [extract_svg_links()].
#' @param gauge Character. Node name to start from (e.g.
#'   `"Bedford Brackley FMP"`).
#' @param direction Character. `"upstream"` (default) follows edges backward;
#'   `"downstream"` follows edges forward.
#'
#' @return A list with `$edges` and `$nodes` [data.table]s.
#'
#' @examples
#' upstream <- subset_network(result, "Bedford Brackley FMP", "upstream")
#' upstream$nodes
#' plot_network_interactive(upstream, title = "Upstream of Bedford Brackley FMP")
#'
#' @export
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


#' List all nodes downstream of a gauge
#'
#' Convenience wrapper around [igraph::subcomponent()] with
#' `mode = "out"`.
#'
#' @param g An [igraph] object returned by [build_graph()] or
#'   [plot_network()].
#' @param gauge Character. Node name to start from.
#'
#' @return Character vector of downstream node names (excluding the
#'   starting gauge itself).
#'
#' @examples
#' g <- build_graph(result)
#' downstream_of(g, "Brackley RG")
#'
#' @export
downstream_of <- function(g, gauge) {
  v <- which(V(g)$name == gauge)
  if (length(v) == 0) stop("No node named: ", gauge)
  setdiff(names(subcomponent(g, v, mode = "out")), gauge)
}


# =============================================================================
# Usage
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
