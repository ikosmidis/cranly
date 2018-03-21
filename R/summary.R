#' Compute a range of author and package network statistics
#'
#' @aliases summary_cranly_network
#'
#' @param object a \code{\link{cranly_network}} object
#' @param ... currently not used
#'
#' @details
#'
#'
#'
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
