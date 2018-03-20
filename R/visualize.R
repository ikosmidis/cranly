#' Visualise a \code{\link{cranly_network}}
#'
#' @export
visualize.cranly_network <- function(object,
                                     package = "cranly",
                                     author = "Ioannis Kosmidis",
                                     physics_threshold = 200,
                                     height = NULL, #"1080px",
                                     width = NULL, #"1080px",
                                     edge_type = c("Imports", "Suggests", "Enhances", "Depends"),
                                     dragNodes = TRUE,
                                     dragView = TRUE,
                                     zoomView = TRUE,
                                     exact = TRUE,...) {

    object <- subset(object, package = package, author = author, edge_type = edge_type, exact = exact)

    if (nrow(object$nodes) == 0) {
            message("Nothing to visualise")
            return(invisible(NULL))
    }

    edges_subset <- object$edges
    nodes_subset <- object$nodes
    colors <- colorspace::diverge_hcl(10, c = 100, l = c(50, 100), power = 1)

    perspective <- attr(object, "perspective")
    keep <- attr(object, "keep")


    if (perspective == "package") {
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
        nodes_subset <- within(nodes_subset, {
            color <- ifelse(Package %in% keep, colors[1], colors[5])
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
        edges_subset <- within(edges_subset, {
            title <- paste("collaborate in:", Package)
            color <- colors[1]
        })

        format_fun <- function(vec) {
            n_items <- length(vec)
            n_full_rows <- n_items %/% 4
            n_last_row <- n_items %% 4
            ind <- c(if (n_full_rows > 0) rep(seq.int(n_full_rows), each = 4) else NULL,
                     rep(n_full_rows + 1, n_last_row))
            paste(tapply(vec, ind, function(x) paste(x, collapse = ", ")), collapse = "<br>")
        }

        nodes_subset <- within(nodes_subset, {
            color <- ifelse(Author %in% keep, colors[1], colors[5])
            label <- Author
            id <- Author
            title <- paste0("Author: ", Author, "<br>",
                            n_collaborators, " collaborators in ",
                            unlist(lapply(nodes_subset$Package, length)),
                            " packages: <br>", unlist(lapply(nodes_subset$Package, format_fun)))
        })

    }

    visNetwork::visNetwork(nodes_subset, edges_subset, height = height, width = width) %>%
        visNetwork::visEdges(arrows = if (perspective == "author") NULL else list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                             physics = nrow(nodes_subset) < physics_threshold) %>%
            visNetwork::visOptions(highlightNearest = TRUE) %>%
            visNetwork::visInteraction(dragNodes = dragNodes, dragView = dragView, zoomView = zoomView) %>%
            visNetwork::visExport()
}


