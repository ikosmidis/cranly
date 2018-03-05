as.igraph.cranly_network <- function(x, ...) {
    edges <- x$edges
    graph::graph(as.matrix(edges[c("from", "to")])) %>%  set_edge_attr("type", value = edges$type)
}

## see http://kateto.net/networks-r-igraph for intro
