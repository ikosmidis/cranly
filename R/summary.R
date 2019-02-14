# Copyright (C) 2018 Ioannis Kosmidis

#' Compute a range of package directives and collaboration network statistics
#'
#' @aliases summary_cranly_network
#'
#' @param object a \code{\link{cranly_network}} object
#' @param advanced logical. If \code{FALSE} (default) only basic network statistics are computed; if \code{TRUE} advanced statistics are also included in the computation (see Details).
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
#' @details
#'
#' If \code{attr(object, "perspective")} is \code{"package"} then the
#' resulting \code{data.frame} will have the following variables:
#' \itemize{
#' \item package. package name
#' \item n_authors (basic). number of authors for the package
#' \item n_imports (basic). number of packages the package imports
#' \item n_imported_by (basic). number of times the package is imported by other packages
#' \item n_suggests (basic). number of packages the package suggests
#' \item n_suggested_by (basic). number of times the package is suggested by other packages
#' \item n_depends (basic). number of packages the package depends on
#' \item n_depended_by (basic). number of packages that have the package as a dependency
#' \item n_enhances (basic). number of packages the package enhances
#' \item n_enhanced_by (basic). number of packages the package is enhanced by
#' \item n_linking_to (basic). number of packages the package links to
#' \item n_linked_by (basic). number of packages the package is linked by
#' \item betweenness (advanced). the package betweenness in the package network; as computed by \code{\link[igraph]{betweenness}}
#' \item closeness (advanced). the closeness centrality of the package in the package network; as computed by \code{\link[igraph]{closeness}}
#' \item page_rank (advanced). the Google PageRank of the package in the package network; as computed by \code{\link[igraph]{page_rank}}
#' \item degree (advanced). the degree of the package in the package network;  as computed by \code{\link[igraph]{degree}}
#' \item eigen_centrality (advanced). the eigenvector centrality score of the package in the package network; as computed by \code{\link[igraph]{eigen_centrality}}
#' }
#'
#' If \code{attr(object, "perspective")} is \code{"author"} then the
#' resulting \code{data.frame} will have the following variables:
#' \itemize{
#' \item author. author name
#' \item n_packages (basic). number of packages the author appears in the package authors
#' \item n_collaborators (basic). total number of co-authors the author has in CRAN
#' \item betweenness (advanced). the author betweenness in the author network; as computed by \code{\link[igraph]{betweenness}}
#' \item closeness (advanced). the closeness centrality of the author in the author network; as computed by \code{\link[igraph]{closeness}}
#' \item page_rank (advanced). the Google PageRank of the author in the author network; as computed by \code{\link[igraph]{page_rank}}
#' \item degree (advanced). the degree of the author in the author network;  as computed by \code{\link[igraph]{degree}}; same as n_collaborators
#' \item eigen_centrality (advanced). the eigenvector centrality score of the author in the author network; as computed by \code{\link[igraph]{eigen_centrality}}
#' }
#' @export
summary.cranly_network <- function(object, advanced = TRUE, ...) {

    perspective <- attr(object, "perspective")
    cranly_graph <- as.igraph.cranly_network(object, reverse = TRUE)

    if (advanced) {
        bet <- betweenness(cranly_graph, normalized = FALSE)
        clo <- closeness(cranly_graph, normalized = FALSE)
        pg_rank <- page_rank(cranly_graph)
        degree <- degree(cranly_graph, normalized = FALSE)
        eigen_cent <- eigen_centrality(cranly_graph, scale = FALSE)
    }

    if (perspective == "package") {

        gr <- subgraph.edges(graph = cranly_graph, eids = which(E(cranly_graph)$type == "imports"),
                             delete.vertices = FALSE)
        n_imports <- degree(gr, mode = "out")
        n_imported_by <- degree(gr, mode = "in")
        gr <- subgraph.edges(graph = cranly_graph, eids = which(E(cranly_graph)$type == "depends"),
                             delete.vertices = FALSE)
        n_depends <- degree(gr, mode = "out")
        n_depended_by <- degree(gr, mode = "in")
        gr <- subgraph.edges(graph = cranly_graph, eids = which(E(cranly_graph)$type == "suggests"),
                             delete.vertices = FALSE)
        n_suggests <- degree(gr, mode = "out")
        n_suggested_by <- degree(gr, mode = "in")
        gr <- subgraph.edges(graph = cranly_graph, eids = which(E(cranly_graph)$type == "enhances"),
                             delete.vertices = FALSE)
        n_enhanced_by <- degree(gr, mode = "out")
        n_enhances <- degree(gr, mode = "in")
        gr <- subgraph.edges(graph = cranly_graph, eids = which(E(cranly_graph)$type == "linkingto"),
                             delete.vertices = FALSE)
        n_linking_to <- degree(gr, mode = "out")
        n_linked_by <- degree(gr, mode = "in")

        package <- object$nodes$package
        n_authors <- unlist(lapply(object$nodes$author, function(x) {l <- length(x); ifelse(l, l, NA)}))
        out <- with(object$nodes,
                    data.frame(package = package,
                               n_authors = n_authors,
                               n_imports = n_imports,
                               n_imported_by = n_imported_by,
                               n_suggests = n_suggests,
                               n_suggested_by = n_suggested_by,
                               n_depends = n_depends,
                               n_depended_by = n_depended_by,
                               n_enhances = n_enhances,
                               n_enhanced_by = n_enhanced_by,
                               n_linking_to = n_linking_to,
                               n_linked_by = n_linked_by,
                               betweenness = if (advanced) bet[package] else NA,
                               closeness = if (advanced)clo[package] else NA,
                               page_rank = if (advanced) pg_rank$vector[package] else NA,
                               degree = if (advanced) degree else NA,
                               eigen_centrality = if (advanced) eigen_cent$vector[package] else NA,
                               stringsAsFactors = FALSE))
    }
    else {
        aut <- object$nodes$author

        n_collaborators <- degree(cranly_graph, mode = "all")

        n_packages <- unlist(lapply(object$nodes$package, function(x) {l <- length(x); ifelse(l, l, NA)}))
        out <- data.frame(author = aut,
                          n_packages = n_packages,
                          ## n_collaborators = object$nodes$n_collaborators,
                          n_collaborators = n_collaborators,
                          betweenness = if (advanced) bet[aut] else NA,
                          closeness = if (advanced) clo[aut] else NA,
                          page_rank = if (advanced) pg_rank$vector[aut] else NA,
                          degree = if (advanced) degree[aut] else NA,
                          eigen_centrality = if (advanced) eigen_cent$vector[aut] else NA,
                          stringsAsFactors = FALSE)
    }
    class(out) <- c("summary_cranly_network", class(out))
    attr(out, "perspective") <- perspective
    attr(out, "timestamp") <- attr(object, "timestamp")
    out
}
