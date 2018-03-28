# Copyright (C) 2018 Ioannis Kosmidis

#' Subset a \code{\link{cranly_network}} according to author, package and/or directive
#'
#' @param x a \code{\link{cranly_network}} object
#' @param package a vector of character strings with the package names to be matched
#' @param author a vector of character strings with the author names to be matched
#' @param directive a vector of at least one of \code{"Imports"}, \code{"Suggests"}, \code{"Enhances"}, \code{"Depends"}
#' @param exact logical. Should we use exact matching? Default is \code{TRUE}
#' @param only logical. If \code{TRUE} the subset includes only the edges between packages named in \code{package} and/or authors named in \code{author}. If \code{FALSE} (default) edges to and from all other packages and/or authors that are linked to \code{package} and/or \code{author} are included in the subset
#' @param ... currently not used
#'
#' @export
subset.cranly_network <- function(x,
                                  package = NULL,
                                  author = NULL,
                                  directive = c("imports", "suggests", "enhances", "depends", "linkingto"),
                                  exact = TRUE,
                                  only = FALSE,
                                  ...) {
    perspective <- attr(x, "perspective")
    if (perspective == "package") {
        p1 <- package_with(x, name = package, exact = exact)
        p2 <- package_by(x, author = author, exact = exact)

        keep <- unique(c(p1, p2))
        if (only) {
            edges_subset <- subset(x$edges, (to %in% keep & from %in% keep) & (type %in% directive))
        }
        else {
            edges_subset <- subset(x$edges, (to %in% keep | from %in% keep) & (type %in% directive))
        }
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), keep))
        nodes_subset <- subset(x$nodes, package %in% node_names)

    }
    else {
        a1 <- author_with(x, name = author, exact = exact)
        a2 <- author_of(x, package = package, exact = exact)
        keep <- unique(c(a1, a2))
        if (only) {
            edges_subset <- subset(x$edges, (to %in% keep & from %in% keep))
        }
        else {
            edges_subset <- subset(x$edges, (to %in% keep | from %in% keep))
        }
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), keep))
        nodes_subset <- subset(x$nodes, author %in% node_names)
    }
    out <- list(edges = edges_subset, nodes = nodes_subset)
    attr(out, "timestamp") <- attr(x, "timestamp")
    attr(out, "perspective") <- perspective

    attr(out, "keep") <- keep
    class(out) <- c("cranly_network", class(out))
    out

}
