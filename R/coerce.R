as.igraph.cranly_network <- function(x, ...) {
    edges <- x$edges
    g <- igraph::graph.edgelist(as.matrix(edges[c("from", "to")]))
    E(g)$type <-  str_replace_all(edges$type,
                                  c("Imports" = "is imported by",
                                    "Depends" = "is dependency of",
                                    "Suggests" = "is suggested by",
                                    "Enhances" = "enhances"))
    V(g)$version <- nodes$Version[match(V(g), nodes$Package)]

}

## see http://kateto.net/networks-r-igraph for intro
