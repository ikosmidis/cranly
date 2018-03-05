#' Visualise a cranly_bd_network
#' @export
#' @examples
#'
visualize.cranly_db_network <- function(object,
                                        packages = "MASS",
                                        physics_threshold = 300,
                                        height = "800px",
                                        width = "800px",
                                        types = c("Imports", "Suggests", "Enhances", "Depends")) {

    edges <- object[[1]]
    nodes <- object[[2]]

    colors <- diverge_hcl(10, c = 100, l = c(50, 100), power = 1)


    edges_subset <- edges %>%
        filter(to %in% packages | from %in% packages) %>%
        filter(type %in% types)

    node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), packages))

    cat("Number of nodes to be plotted:", length(node_names), "\n",
        "Number of edges to be plotted:", nrow(edges_subset), "\n")

    edges_subset <- edges_subset %>% mutate(color = str_replace_all(type,
                                       c("Imports" = colors[10],
                                         "Depends" = colors[10],
                                         "Suggests" = colors[4],
                                         "Enhances" = colors[4])),
               dashes = ifelse(type %in% c("Imports", "Depends", "Suggests"), FALSE, TRUE),
               title = str_replace_all(type,
                                       c("Imports" = "is imported by",
                                         "Depends" = "is dependency of",
                                         "Suggests" = "is suggested by",
                                         "Enhances" = "enhances")))

    ## Imports includes depends
    nodes_subset <- nodes %>%
        filter(Package %in% node_names) %>%
        mutate(color = ifelse(Package %in% packages, colors[1], colors[5]),
               label = Package,
               title = paste0("<a href=https://CRAN.R-project.org/package=", Package, ">", Package, "</a> (", Version, ")<br>",
                              "imports/imported by:", n_imports, "/", n_imported_by, "<br>",
                              "depends/is dependency of:", n_depends, "/", n_depended_by, "<br>",
                              "suggests/suggested by:", n_suggests, "/", n_suggested_by, "<br>",
                              "enhances/enhaced by:", n_enhances, "/", n_enhanced_by, "<br>",
                              "<img src=https://cranlogs.r-pkg.org/badges/", Package, "?color=969696>")) %>%
        rename(id = Package)

    visNetwork(nodes_subset, edges_subset, height = height, width = width) %>%
        visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 0.5)),
                 physics = nrow(nodes_subset) < physics_threshold) %>%
        visOptions(highlightNearest = TRUE)
    ## visNodes(shape = "icon", icon = list(face ='FontAwesome', code = "f187")) %>%
    ## addFontAwesome()
}
