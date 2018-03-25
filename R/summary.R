#' Compute a range of package directives and collaboration network statistics
#'
#' @aliases summary_cranly_network
#'
#' @param object a \code{\link{cranly_network}} object
#' @param ... currently not used
#'
#' @return
#'
#' A \code{\link{data.frame}} of various statistics for the author collaboration
#' network or the package directives network, depending on whether
#' \code{attr(object, "perspective")} is \code{"author"} or
#' \code{"package"}, respectively. See Details for the current list of
#' statistics returned.
#'
#'
#' If \code{attr(object, "perspective")} is \code{"package"} then the
#' resulting \code{data.frame} will have the following variables:
#' \itemize{
#' \item package. package name
#' \item n_authors. number of authors for the package
#' \item n_imports. number of packages the package imports
#' \item n_imported_by. number of times the package is imported by other packages
#' \item n_suggests. number of packages the package suggests
#' \item n_suggested_by. number of times the package is suggested by other packages
#' \item n_depends. number of packages the package depends on
#' \item n_depended_by. number of packages that have the package as a dependency
#' \item n_enhances. number of packages the package enhances
#' \item n_enhanced_by. number of packages the package is enhanced by
#' \item betweenness. the package betweenness in the package network; as computed by \code{\link[igraph]{betweenness}}
#' \item closeness. the closeness centrality of the package in the package network; as computed by \code{\link[igraph]{closeness}}
#' \item page_rank. the Google PageRank of the package in the package network; as computed by \code{\link[igraph]{page_rank}}
#' \item degree. the degree of the package in the package network;  as computed by \code{\link[igraph]{degree}}
#' \item eigen_centrality. the eigenvector centrality score of the package in the package network; as computed by \code{\link[igraph]{eigen_centrality}}
#' }
#'
#' If \code{attr(object, "perspective")} is \code{"author"} then the
#' resulting \code{data.frame} will have the following variables:
#' \itemize{
#' \item author. author name
#' \item n_packages. number of packages the author appears in the package authors
#' \item n_collaborators. total number of co-authors the author has in CRAN
#' \item betweenness. the author betweenness in the author network; as computed by \code{\link[igraph]{betweenness}}
#' \item closeness. the closeness centrality of the author in the author network; as computed by \code{\link[igraph]{closeness}}
#' \item page_rank. the Google PageRank of the author in the author network; as computed by \code{\link[igraph]{page_rank}}
#' \item degree. the degree of the author in the author network;  as computed by \code{\link[igraph]{degree}}; same as n_collaborators
#' \item eigen_centrality. the eigenvector centrality score of the author in the author network; as computed by \code{\link[igraph]{eigen_centrality}}
#' }
#' @export
summary.cranly_network <- function(object, ...) {

    perspective <- attr(object, "perspective")
    cranly_graph <- as.igraph.cranly_network(object, reverse = TRUE)

    bet <- betweenness(cranly_graph, normalized = FALSE)
    clo <- closeness(cranly_graph, normalized = FALSE)
    pg_rank <- page_rank(cranly_graph)
    degree <- degree(cranly_graph, normalized = FALSE)
    eigen_cent <- eigen_centrality(cranly_graph, scale = FALSE)

    if (perspective == "package") {
        package <- object$nodes$package
        n_authors <- unlist(lapply(object$nodes$author, function(x) {l <- length(x); ifelse(l, l, NA)}))

        out <- with(object$nodes,
                    data.frame(package = package,
                               n_authors = n_authors,
                               n_imports = ifelse(is.na(n_authors), NA, n_imports),
                               n_imported_by = n_imported_by,
                               n_suggests = ifelse(is.na(n_authors), NA, n_suggests),
                               n_suggested_by = n_suggested_by,
                               n_depends = ifelse(is.na(n_authors), NA, n_depends),
                               n_depended_by = n_depended_by,
                               n_enhances = ifelse(is.na(n_authors), NA, n_enhances),
                               n_enhanced_by = n_enhanced_by,
                               betweenness = bet[package],
                               closeness = clo[package],
                               page_rank = pg_rank$vector[package],
                               degree = degree,
                               eigen_centrality = eigen_cent$vector[package],
                               stringsAsFactors = FALSE))
    }
    else {
        aut <- object$nodes$author

        n_packages <- unlist(lapply(object$nodes$package, function(x) {l <- length(x); ifelse(l, l, NA)}))
        out <- data.frame(author = aut,
                          n_packages = n_packages,
                          n_collaborators = object$nodes$n_collaborators,
                          betweenness = bet[aut],
                          closeness = clo[aut],
                          page_rank = pg_rank$vector[aut],
                          degree = degree[aut],
                          eigen_centrality = eigen_cent$vector[aut],
                          stringsAsFactors = FALSE)
    }
    class(out) <- c("summary_cranly_network", class(out))
    attr(out, "perspective") <- perspective
    attr(out, "timestamp") <- attr(object, "timestamp")
    out
}
