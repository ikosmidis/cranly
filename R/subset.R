#' @export

subset.cranly_network <- function(x,
                                  package = "cranly",
                                  author = "Ioannis Kosmidis",
                                  edge_type = c("Imports", "Suggests", "Enhances", "Depends"),
                                  exact = TRUE,
                                  ...) {
    perspective <- attr(x, "perspective")
    if (perspective == "package") {
        p1 <- package_with(x, name = package, exact = exact)
        p2 <- package_by(x, author = author, exact = exact)
        keep <- unique(c(p1, p2))
        edges_subset <- subset(x$edges, (to %in% keep | from %in% keep) & (type %in% edge_type))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), keep))
        nodes_subset <- subset(x$nodes, Package %in% node_names)
    }
    else {
        a1 <- author_with(x, name = author, exact = exact)
        a2 <- author_of(x, package = package, exact = exact)
        keep <- unique(c(a1, a2))
        edges_subset <- subset(x$edges, (to %in% keep | from %in% keep))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), keep))
        nodes_subset <- subset(x$nodes, Author %in% node_names)
    }
    out <- list(edges = edges_subset, nodes = nodes_subset)
    attr(out, "timestamp") <- attr(x, "timestamp")
    attr(out, "perspective") <- perspective

    attr(out, "keep") <- keep
    class(out) <- c("cranly_network", class(out))
    out

}
