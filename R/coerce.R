# Copyright (C) 2018- Ioannis Kosmidis

#' Coerce a [`cranly_network`] to an [`igraph::graph`] object
#'
#' @param x a [`cranly_network`] object.
#' @param reverse logical. Should the direction of the edges be reversed? See details. Default is [`TRUE`].
#' @param ... currently not used.
#'
#' @details
#'
#' The convention for a [`cranly_network`] object with `perspective =
#' "package"` is that the direction of an edge is from the package
#' that is imported by, suggested by, enhances or is a dependency of
#' another package, to the latter package. `reverse` reverses that
#' direction to correctly compute relevant network summaries (see
#' `summary.cranly_network`). `reverse` is only relevant when the
#' `attr(x, "perspective")` is `"package"` and is ignored when
#' `attr(x, "perspective")` is `"author"`, in which case the resulting
#' [`igraph::graph`] object represents an undirected network of
#' authors.
#'
#' @examples
#' \donttest{
#'
#' data("crandb", package = "cranly")
#' # Package directives network
#' package_network <- build_network(object = crandb, perspective = "package")
#' igraph::as.igraph(package_network)
#'
#' ## Author collaboration network
#' author_network <- build_network(object = crandb, perspective = "author")
#' igraph::as.igraph(author_network)
#'
#' }
#' @export
as.igraph.cranly_network <- function(x, reverse = FALSE, ...) {

    perspective <- attr(x, "perspective")

    edges <- x$edges
    nodes <- x$nodes

    if (perspective == "package") {
        v_names <- c("package", "version", "author", "date", "url", "license", "maintainer")
        e_inds <- if (reverse) c(2, 1, 3:ncol(edges)) else 1:ncol(edges)
        g <- graph.data.frame(edges[, e_inds], vertices = nodes[v_names], directed = TRUE)
        E(g)$type <-  edges$type
    }
    else {
	vnames <- c("author", "package")
        g <- graph.data.frame(edges, vertices = nodes[vnames], directed = FALSE)
        E(g)$package <- edges$package
        E(g)$imports <- edges$imports
        E(g)$depends <- edges$depends
        E(g)$suggests <- edges$suggests
        E(g)$enhances <- edges$enhances
        E(g)$linking_to <- edges$linking_to
        E(g)$version <- edges$version
    }
    g

}
