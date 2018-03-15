#' Summarize a \code{\link{cranly_network}}
#'
#' @examples
#'
#' @export
summary.cranly_network <- function(object,
                                   normalize = FALSE,
                                   ...) {

    perspective <- attr(object, "perspective")
    cranly_graph <- as.igraph.cranly_network(object)

    bet <- betweenness(cranly_graph, normalized = normalize)
    clo <- closeness(cranly_graph, normalized = normalize)
    pg_rank <- page_rank(cranly_graph)
    degree <- degree(cranly_graph, normalized = normalize)
    eigen_cent <- eigen_centrality(cranly_graph, scale = normalize)

    if (perspective == "package") {
        package <- object$nodes$Package
        n_authors <- unlist(lapply(object$nodes$Author, function(x) {l <- length(x); ifelse(l, l, NA)}))
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
                              stringsAsFactors = TRUE))
    }
    else {
        aut <- object$nodes$Author
        n_packages <- unlist(lapply(object$nodes$Package, function(x) {l <- length(x); ifelse(l, l, NA)}))
        out <- data.frame(author = aut,
                          n_packages = n_packages,
                          betweenness = bet[aut],
                          closeness = clo[aut],
                          page_rank = pg_rank$vector[aut],
                          degree = degree[aut],
                          eigen_centrality = eigen_cent$vector[aut],
                          stringsAsFactors = TRUE)
    }
    out

}
