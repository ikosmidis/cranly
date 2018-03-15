#' Summarize a \code{\link{cranly_network}}
#'
#' @examples
#'
#' @export
summary.cranly_network <- function(object,
                                   edge_type = c("Imports", "Suggests", "Enhances", "Depends"),
                                   normalize = FALSE) {

    perspective <- attr(object, "perspective")
    cranly_graph <- as.igraph.cranly_network(object)
    if (perspective == "package") {
        cranly_graph <- subgraph.edges(cranly_graph, eids = which(E(cranly_graph)$type %in% edge_type))
    }

    bet <- igraph::betweenness(cranly_graph, normalized = normalize)
    clo <- igraph::closeness(cranly_graph, normalized = normalize)
    pg_rank <- igraph::page_rank(cranly_graph)
    degree <- igraph::degree(cranly_graph, normalized = normalize)
    eigen_cent <- igraph::eigen_centrality(cranly_graph, scale = normalize)

    if (perspective == "package") {
        package <- names(bet)
        out <- data.frame(package = package,
                          betweenness = bet,
                          closeness = clo[package],
                          page_rank = pg_rank$vector[package],
                          degree = degree,
                          eigen_centrality = eigen_cent$vector[package],
                          stringsAsFactors = TRUE)
    }
    else {
        aut <- names(bet)
        out <- data.frame(author = aut,
                          betweenness = bet,
                          closeness = clo[aut],
                          page_rank = pg_rank$vector[aut],
                          degree = degree[aut],
                          eigen_centrality = eigen_cent$vector[aut],
                          stringsAsFactors = TRUE)
    }
    out

}