#' Visualise a \code{\link{cranly_network}}
#'
#' @examples
#'
#' @export
visualize.cranly_network <- function(object,
                                     packages = "MASS",
                                     authors = "Brian Ripley",
                                     physics_threshold = 200,
                                     height = "1080px",
                                     width = "1080px",
                                     types = c("Imports", "Suggests", "Enhances", "Depends")) {
    edges <- object[[1]]
    nodes <- object[[2]]
    colors <- colorspace::diverge_hcl(10, c = 100, l = c(50, 100), power = 1)

    perspective <- attr(object, "perspective")

    if (perspective == "package") {
        edges_subset <- subset(edges, (to %in% packages | from %in% packages) & (type %in% types))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), packages))
        cat("Number of nodes to be plotted:", length(node_names), "\n")
        cat("Number of edges to be plotted:", nrow(edges_subset), "\n")

        edges_subset <- within(edges_subset, {
            color <- str_replace_all(type,
                                     c("Imports" = colors[10],
                                       "Depends" = colors[10],
                                       "Suggests" = colors[4],
                                       "Enhances" = colors[4]))
            dashes <- ifelse(type %in% c("Imports", "Depends", "Suggests"), FALSE, TRUE)
            title <- str_replace_all(type,
                                     c("Imports" = "is imported by",
                                       "Depends" = "is dependency of",
                                       "Suggests" = "is suggested by",
                                       "Enhances" = "enhances"))
        })
        nodes_subset <- subset(nodes, Package %in% node_names)
        nodes_subset <- within(nodes_subset, {
            color <- ifelse(Package %in% packages, colors[1], colors[5])
            label <- Package
            id <- Package
            title <- paste0("<a href=https://CRAN.R-project.org/package=", Package, ">", Package, "</a> (", Version, ")<br>",
                            "imports/imported by:", n_imports, "/", n_imported_by, "<br>",
                            "depends/is dependency of:", n_depends, "/", n_depended_by, "<br>",
                            "suggests/suggested by:", n_suggests, "/", n_suggested_by, "<br>",
                            "enhances/enhaced by:", n_enhances, "/", n_enhanced_by, "<br>",
                            "<img src=https://cranlogs.r-pkg.org/badges/", Package, "?color=969696>")
        })
    }
    else {
        edges_subset <- subset(edges, (to %in% authors | from %in% authors))
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), authors))
        cat("Number of nodes to be plotted:", length(node_names), "\n")
        cat("Number of edges to be plotted:", nrow(edges_subset), "\n")

        edges_subset <- within(edges_subset, {
            title <- Package
        })

        nodes_subset <- subset(nodes, Author %in% node_names)

        nodes_subset <- within(nodes_subset, {
            label <- Author
            id <- Author
            title <- paste0(Author, "<br>",
                            "Packages: ", unlist(lapply(nodes_subset$Package, paste, collapse = ", ")), "<br>")
        })

    }

    visNetwork::visNetwork(nodes_subset, edges_subset, height = height, width = width) %>%
        visNetwork::visEdges(arrows = if(perspective == "author") NULL else list(to = list(enabled = TRUE,  scaleFactor = 0.5)),
                             physics = nrow(nodes_subset) < physics_threshold) %>%
            visNetwork::visOptions(highlightNearest = TRUE)
}
