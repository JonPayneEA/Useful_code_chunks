# =============================================================================
# extract_svg_links.R
# 
# Extract directed links from a flood-network SVG diagram, with node names.
# 
# Each node’s display label lives in the last <tspan> of its <text> element:
# - Normal nodes: first tspan is the type (FMP, PDM, CatAvg, …), second
# is the name (e.g. "Bedford Tove FMP").
# - Import nodes: a single tspan holds the name (e.g. "Foxcote T").
# 
# Notes on portability:
# * Regex literals use R 4.0+ raw strings r"(…)" to avoid backslash
# escaping issues on copy/paste.
# * XPath literals are built from intToUtf8(39L) so no apostrophe character
# appears in the source code.
# =============================================================================

library(xml2)
library(data.table)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

extract_svg_links <- function(svg_file, tol = 5) {
  doc <- read_xml(svg_file)
  ns  <- c(s = "http://www.w3.org/2000/svg")
  q        <- intToUtf8(39L)
  xp_nodes <- paste0("//s:g[contains(@class,", q, "node", q, ")]")
  xp_edges <- paste0("//s:g[@id=", q, "edges", q, "]/s:g")
  
  # –– 1. Node ports —————————————————––
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
  
  node_groups <- xml_find_all(doc, xp_nodes, ns)
  if (length(node_groups) == 0) stop("No <g class='node ...'> elements found.")
  
  ports <- rbindlist(lapply(node_groups, function(g) {
    node_id    <- xml_attr(g, "id")
    node_class <- xml_attr(g, "class")
    node_name  <- node_label(g)
    tr         <- parse_translate(xml_attr(g, "transform"))
    
    circles <- xml_find_all(g, ".//s:circle", ns)
    if (length(circles) > 0) {
      parents <- xml_parent(circles)
      return(data.table(
        node_id    = node_id,
        node_name  = node_name,
        node_class = node_class,
        port_id    = xml_attr(parents, "id"),
        x          = tr[1] + as.numeric(xml_attr(circles, "cx")),
        y          = tr[2] + as.numeric(xml_attr(circles, "cy")),
        fill       = xml_attr(circles, "fill")
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
      port_id    = node_id,
      x          = tr[1] + mean(xs),
      y          = tr[2] + max(ys),
      fill       = xml_attr(polys[[1]], "style")
    )

  }), fill = TRUE)
  
  # –– 2. Edge endpoints —————————————————
  
  edge_groups <- xml_find_all(doc, xp_edges, ns)
  if (length(edge_groups) == 0) stop("No edges found under <g id='edges'>.")
  
  edges <- rbindlist(lapply(seq_along(edge_groups), function(i) {
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
      stroke  = stroke,
      dashed  = grepl("dasharray", style)
    )

  }))
  
  # –– 3. Match endpoints to nearest port ———————————
  nearest <- function(x, y) {
    d <- sqrt((ports$x - x)^2 + (ports$y - y)^2)
    i <- which.min(d)
    list(ports$node_id[i], ports$node_name[i], ports$port_id[i],
         ports$node_class[i], d[i])
  }
  
  edges[, c("from_node", "from_name", "from_port", "from_class", "from_dist") :=
          nearest(start_x, start_y), by = edge_id]
  edges[, c("to_node", "to_name", "to_port", "to_class", "to_dist") :=
          nearest(end_x, end_y), by = edge_id]
  
  edges[, suspect := (from_dist > tol) | (to_dist > tol)]
  
  list(
    ports = ports,
    edges = edges[, .(edge_id,
                      from_name, from_class, from_node, from_port,
                      to_name,   to_class,   to_node,   to_port,
                      stroke, dashed, suspect, from_dist, to_dist)]
  )
}

result <- extract_svg_links("bedford_ouse.svg")
result$edges
result$ports

# Quality check: any endpoints that did not snap onto a port?
result$edges[suspect == TRUE]

# Persist
# fwrite(result$edges, "links.csv")

# Convert to igraph for downstream analysis
# library(igraph)
# g <- graph_from_data_frame(
#   d        = result$edges[, .(from = from_node, to = to_node, stroke, dashed)],
#   vertices = unique(result$ports[, .(node_id, node_class)]),
#   directed = TRUE
# )
# 
# plot(g)

