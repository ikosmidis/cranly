#' Interactive visualization of package(s) dependence tree from a [cranly_network()]
#'
#' @inheritParams plot.cranly_network
#' @param x a [cranly_dependence_tree()] object
#'
#' @seealso compute_dependence_tree dependence_tree
#'
#' @export
plot.cranly_dependence_tree <- function(x,
                                        physics_threshold = 200,
                                        height = NULL, #"1080px",
                                        width = NULL, #"1080px",
                                        dragNodes = TRUE,
                                        dragView = TRUE,
                                        zoomView = TRUE,
                                        legend = TRUE,
                                        title = TRUE,
                                        plot = TRUE,
                                        ...) {


    if (nrow(x$nodes) == 0) {
            message("Nothing to plot")
            return(invisible(NULL))
    }

    timestamp <- attr(x, "timestamp")
    summaries <- x$summaries

    edges <- x$edges
    nodes <- x$nodes

    n_colors <- abs(min(nodes$generation))

    colors <- c("#D33F6A", colorspace::sequential_hcl(n_colors, c = 100, l = c(50, 100), power = 1))

    edges <- within(edges, {
        color <- str_replace_all(type,
                                 c("imports" = "#D33F6A",
                                   "depends" = "#D33F6A",
                                   "suggests" = "#C7CEF5",
                                   "enhances" = "#C7CEF5",
                                   "linkingto" = "#F9C2CB"))
        dashes <- ifelse(type %in% c("imports", "depends", "suggests"), FALSE, TRUE)
        title <- str_replace_all(type,
                                 c("imports" = "is imported by",
                                   "depends" = "is dependency of",
                                   "suggests" = "is suggested by",
                                   "enhances" = "enhances",
                                   "linkingto" = "is linked by"))
    })
    summaries <- summaries[nodes$package, ]

    nodes <- within(nodes, {
        color <- colors[abs(generation) + 1]
        label <- package
        id <- package
        title <- paste0("<a href=https://CRAN.R-project.org/package=", package, ">", package, "</a> (", version, ")<br>",
                        "Generation: ", generation, "<br>",
                        "Maintainer: ", maintainer, "<br>",
                        "imports/imported by:", summaries$n_imports, "/", summaries$n_imported_by, "<br>",
                        "depends/is dependency of:", summaries$n_depends, "/", summaries$n_depended_by, "<br>",
                        "suggests/suggested by:", summaries$n_suggests, "/", summaries$n_suggested_by, "<br>",
                        "enhances/enhaced by:", summaries$n_enhances, "/", summaries$n_enhanced_by, "<br>",
                        "linkingto/linked by:", summaries$n_linking, "/", summaries$n_linked_by, "<br>",
                        "<img src=https://cranlogs.r-pkg.org/badges/", package, "?color=969696>")
    })

    lnodes <- ledges <- main <- NULL
    ## legend
    if (legend) {
        lnodes <- data.frame(label = c(paste("Generation", 0:(-n_colors))),
                             color = colors,
                             font.align = "top")

        ledges <- data.frame(label = c("is imported by", "is dependency of", "is suggested by", "enhances", "is linked by"),
                             color = c("#D33F6A", "#D33F6A", "#C7CEF5", "#C7CEF5", "#F9C2CB"),
                             dashes = c(FALSE, FALSE, FALSE, TRUE, TRUE),
                             arrows = c("to", "to", "to", "to", "to"),
                             font.align = "top")
    }

    if (title) {
        main <- paste(
            paste0("cranly dependence tree for package names with<br> \"", paste(x$package, collapse = "\", \""), "\"", collapse = ""),
            "<br>",
            paste0("CRAN database version<br>", format(timestamp, format = "%a, %d %b %Y, %H:%M"), collapse = ""))
    }

    export_name <- paste0("cranly_dependence_tree-", format(timestamp, format = "%d-%b-%Y"), "-", paste0(x$package, collapse = "-"))

    res <- visNetwork::visNetwork(nodes, edges, height = height, width = width,
                                  main = list(text = main,
                                              style = "font-family:Georgia, Times New Roman, Times, serif;font-size:15px")) %>%
        visNetwork::visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                             physics = nrow(nodes) < physics_threshold) %>%
        visNetwork::visOptions(highlightNearest = TRUE) %>%
        visNetwork::visLegend(addNodes = lnodes, addEdges = ledges, useGroups = FALSE) %>%
        visNetwork::visInteraction(dragNodes = dragNodes, dragView = dragView, zoomView = zoomView) %>%
        visNetwork::visExport(name = export_name, label = "PNG snapshot", style = "")

    if (plot) {
        return(res %>% visNetwork::visHierarchicalLayout(levelSeparation = 50))
    }
    else {
        return(invisible(res))
    }
}
