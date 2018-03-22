#' Coerce a \code{\link{cranly_network}} to an \code{\link[igraph]{graph}} object
#'
#' @param x a \code{\link{cranly_network}} object
#' @param reverse logical. Should the direction of the edges be reversed? See details. Default is \code{TRUE}
#' @param ... currently not used
#'
#' @details
#'
#' The convention for a \code{\link{cranly_network}} object with
#' \code{perspective = "package"} is that the direction of an edge is
#' from the package that is imported by, suggested by, enhances or is
#' a dependency of another package, to the latter
#' package. \code{reverse} reverses that direction to correctly
#' compute relevant network summaries (see
#' \code{summary.cranly_network}). \code{reverse} is only relevant
#' when the \code{attr(x, "perspective")} is "package" and is ignored
#' when \code{attr(x, "perspective")} is "author", in which case the
#' resulting \code{\link[igraph]{graph}} object represents an
#' undirected network of authors.
#'
#' @examples
#' \dontrun{
#'
#' data("cran20032018", package = "cranly")
#' ## Package network
#' package_network <- build_network(object = cran20032018, perspective = "package")
#' igraph::as.igraph(package_network)
#'
#' ## Author network
#' author_network <- build_network(object = cran20032018, perspective = "author")
#' igraph::as.igraph(author_network)
#'
#' }
#' @export
as.igraph.cranly_network <- function(x, reverse = FALSE, ...) {

    perspective <- attr(x, "perspective")

    edges <- x$edges
    nodes <- x$nodes

    if (perspective == "package") {
        v_names <- c("package", "version", "author", "date", "url", "license", "maintainer",
                     "n_imports", "n_imported_by",
                     "n_suggests", "n_suggested_by",
                     "n_depends", "n_depended_by",
                     "n_enhances", "n_enhanced_by")
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
        E(g)$version <- edges$version
    }
    g

}
