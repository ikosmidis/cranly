#' @export

subset.cranly_network <- function(object,
                                  package = NULL,
                                  author = NULL,
                                  edge_type = c("Imports", "Suggests", "Enhances", "Depends")) {
    perspective <- attr(object, "perspective")
    if (perspective == "package") {
        if (is.null(package)) {
            return(object)
        }
        edges_subset <- subset(object$edges, (to %in% package | from %in% package) & (type %in% edge_type))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), package))
        nodes_subset <- subset(object$nodes, Package %in% node_names)
    }
    else {
        if (is.null(author)) {
            return(object)
        }
        edges_subset <- subset(object$edges, (to %in% author | from %in% author))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), author))
        nodes_subset <- subset(object$nodes, Author %in% node_names)
    }
    out <- list(edges = edges_subset, nodes = nodes_subset)
    attr(out, "timestamp") <- attr(object, "timestamp")
    attr(out, "perspective") <- perspective
    class(out) <- c("cranly_network", class(out))
    out

}
