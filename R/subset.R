#' @export

subset.cranly_network <- function(x,
                                  package = NULL,
                                  author = NULL,
                                  edge_type = c("Imports", "Suggests", "Enhances", "Depends"),
                                  ...) {
    perspective <- attr(x, "perspective")
    if (perspective == "package") {
        if (is.null(package)) {
            return(x)
        }
        edges_subset <- subset(x$edges, (to %in% package | from %in% package) & (type %in% edge_type))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), package))
        nodes_subset <- subset(x$nodes, Package %in% node_names)
    }
    else {
        if (is.null(author)) {
            return(x)
        }
        edges_subset <- subset(x$edges, (to %in% author | from %in% author))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), author))
        nodes_subset <- subset(x$nodes, Author %in% node_names)
    }
    out <- list(edges = edges_subset, nodes = nodes_subset)
    attr(out, "timestamp") <- attr(x, "timestamp")
    attr(out, "perspective") <- perspective
    class(out) <- c("cranly_network", class(out))
    out

}
