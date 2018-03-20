#' Visualise a \code{\link{cranly_network}}
#'
#' @export
visualize.cranly_network <- function(object,
                                     package = NULL,
                                     author = NULL,
                                     physics_threshold = 200,
                                     height = NULL, #"1080px",
                                     width = NULL, #"1080px",
                                     directive = c("imports", "suggests", "enhances", "depends"),
                                     dragNodes = TRUE,
                                     dragView = TRUE,
                                     zoomView = TRUE,
                                     exact = TRUE,...) {

    object <- subset(object, package = package, author = author, directive = directive, exact = exact)

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
                                     c("imports" = colors[10],
                                       "depends" = colors[10],
                                       "suggests" = colors[4],
                                       "enhances" = colors[4]))
            dashes <- ifelse(type %in% c("imports", "depends", "suggests"), FALSE, TRUE)
            title <- str_replace_all(type,
                                     c("imports" = "is imported by",
                                       "depends" = "is dependency of",
                                       "suggests" = "is suggested by",
                                       "enhances" = "enhances"))
        })
        nodes_subset <- within(nodes_subset, {
            color <- ifelse(package %in% keep, colors[1], colors[5])
            label <- package
            id <- package
            title <- paste0("<a href=https://CRAN.R-project.org/package=", package, ">", package, "</a> (", version, ")<br>",
                            "Maintainer: ", maintainer, "<br>",
                            "imports/imported by:", n_imports, "/", n_imported_by, "<br>",
                            "depends/is dependency of:", n_depends, "/", n_depended_by, "<br>",
                            "suggests/suggested by:", n_suggests, "/", n_suggested_by, "<br>",
                            "enhances/enhaced by:", n_enhances, "/", n_enhanced_by, "<br>",
                            "<img src=https://cranlogs.r-pkg.org/badges/", package, "?color=969696>")
        })
    }
    else {
        edges_subset <- within(edges_subset, {
            title <- paste("collaborate in:", package)
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
            color <- ifelse(author %in% keep, colors[1], colors[5])
            label <- author
            id <- author
            title <- paste0("Author: ", author, "<br>",
                            n_collaborators, " collaborators in ",
                            unlist(lapply(nodes_subset$package, length)),
                            " packages: <br>", unlist(lapply(nodes_subset$package, format_fun)))
        })

    }

    visNetwork::visNetwork(nodes_subset, edges_subset, height = height, width = width) %>%
        visNetwork::visEdges(arrows = if (perspective == "author") NULL else list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                             physics = nrow(nodes_subset) < physics_threshold) %>%
            visNetwork::visOptions(highlightNearest = TRUE) %>%
            visNetwork::visInteraction(dragNodes = dragNodes, dragView = dragView, zoomView = zoomView) %>%
            visNetwork::visExport()
}


