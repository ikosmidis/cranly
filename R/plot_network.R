# Copyright (C) 2018 Ioannis Kosmidis

#' Interactive visualization of a package or author \code{\link{cranly_network}} using the vis.js library
#'
#' @inheritParams subset.cranly_network
#' @inheritParams summary.cranly_network
#' @inheritParams visNetwork::visNetwork
#' @param physics_threshold integer. How many nodes before switching off physics simulations for edges? Default is \code{200}. See, also \code{\link[visNetwork]{visEdges}}
#' @param dragNodes logical. Should the user be able to drag the nodes that are not fixed? Default is \code{TRUE}
#' @param dragView logical. Should the user be able to drag the view around? Default is \code{TRUE}
#' @param zoomView logical. Should the user be able to zoom in? Default is \code{TRUE}
#' @param legend logical. Should a legend be added on the resulting visualization? Default is \code{FALSE}
#' @param title logical. Should a title be added on the resulting visualization? Default is \code{FALSE}
#' @param global locical. If \code{TRUE} (default) the network summary statistics are computed on \code{object}, otherwise, on the subset of \code{object} according to \code{package}, \code{author}, \code{directive}, \code{base}, \code{recommended}
#' @param ... currently not used
#'
#' @examples
#' \dontrun{
#' data("cran_sample", package = "cranly")
#' package_network <- build_network(cran_sample)
#' ## The package directives network of all users with Ioannis in
#' ## their name from the CRAN database subset cran_sample
#' plot(package_network, author = "Ioannis")
#' ## The package directives network of "Achim Zeileis"
#' plot(package_network, author = "Achim Zeileis")
#'
#' author_network <- build_network(cran_sample, perspective = "author")
#' plot(author_network, author = "Ioannis", title = TRUE)
#' }
#' @export
plot.cranly_network <- function(x,
                                package = NULL,
                                author = NULL,
                                physics_threshold = 200,
                                height = NULL, #"1080px",
                                width = NULL, #"1080px",
                                directive = c("imports", "suggests", "enhances", "depends", "linkingto"),
                                base = TRUE,
                                recommended = TRUE,
                                dragNodes = TRUE,
                                dragView = TRUE,
                                zoomView = TRUE,
                                exact = TRUE,
                                legend = FALSE,
                                title = FALSE,
                                global = TRUE,
                                ...) {

    if (global) {
        summaries <- summary(x, advanced = FALSE)
    }
    x <- subset(x, package = package, author = author, directive = directive, exact = exact,
                base = base, recommended = recommended)

    if (!global) {
        summaries <- summary(x, advanced = FALSE)
    }

    timestamp <- attr(x, "timestamp")

    if (nrow(x$nodes) == 0) {
            message("Nothing to visualise")
            return(invisible(NULL))
    }

    edges_subset <- x$edges
    nodes_subset <- x$nodes
    colors <- colorspace::diverge_hcl(10, c = 100, l = c(50, 100), power = 1)

    perspective <- attr(x, "perspective")
    keep <- attr(x, "keep")

    lnodes <- ledges <- main <- NULL

    if (perspective == "package") {
        edges_subset <- within(edges_subset, {
            color <- str_replace_all(type,
                                     c("imports" = colors[10],
                                       "depends" = colors[10],
                                       "suggests" = colors[4],
                                       "enhances" = colors[4],
                                       "linkingto" = colors[7]))
            dashes <- ifelse(type %in% c("imports", "depends", "suggests"), FALSE, TRUE)
            title <- str_replace_all(type,
                                     c("imports" = "is imported by",
                                       "depends" = "is dependency of",
                                       "suggests" = "is suggested by",
                                       "enhances" = "enhances",
                                       "linkingto" = "is linked by"))
        })
        summaries <- summaries[nodes_subset$package, ]
        nodes_subset <- within(nodes_subset, {
            color <- ifelse(package %in% keep, colors[1], colors[5])
            label <- package
            id <- package
            title <- paste0("<a href=https://CRAN.R-project.org/package=", package, ">", package, "</a> (", version, ")<br>",
                            "Maintainer: ", maintainer, "<br>",
                            "imports/imported by:", summaries$n_imports, "/", summaries$n_imported_by, "<br>",
                            "depends/is dependency of:", summaries$n_depends, "/", summaries$n_depended_by, "<br>",
                            "suggests/suggested by:", summaries$n_suggests, "/", summaries$n_suggested_by, "<br>",
                            "enhances/enhaced by:", summaries$n_enhances, "/", summaries$n_enhanced_by, "<br>",
                            "linkingto/linked by:", summaries$n_linking, "/", summaries$n_linked_by, "<br>",
                            "<img src=https://cranlogs.r-pkg.org/badges/", package, "?color=969696>")
        })

        ## legend
        if (legend) {
            lnodes <- data.frame(label = c("Packages matching query", "Neighbouring packages"),
                                 color = c(colors[1], colors[5]),
                                 font.align = "top")

            ledges <- data.frame(label = c("is imported by", "is dependency of", "is suggested by", "enhances", "is linked by"),
                                 color = c(colors[10], colors[10], colors[4], colors[4], colors[7]),
                                 dashes = c(FALSE, FALSE, FALSE, TRUE, TRUE),
                                 arrows = c("to", "to", "to", "to", "to"),
                                 font.align = "top")
        }

        if (title) {
            main <- paste(
                paste0("CRAN database version<br>", format(timestamp, format = "%a, %d %b %Y, %H:%M"), collapse = ""),
                "<br>",
                if (!is.null(package)) paste0("Package names with<br> \"", paste(package, collapse = "\", \""), "\"", collapse = ""),
                "<br>",
                if (!is.null(author)) paste0("Author names with<br> \"", paste(author, collapse = "\", \""), "\"", collapse = ""))
        }

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
        summaries <- summaries[nodes_subset$author, ]

        nodes_subset <- within(nodes_subset, {
            color <- ifelse(author %in% keep, colors[1], colors[5])
            label <- author
            id <- author
            title <- paste0("Author: ", author, "<br>",
                            summaries$n_collaborators, " collaborators in ",
                            unlist(lapply(nodes_subset$package, length)),
                            " packages: <br>", unlist(lapply(nodes_subset$package, format_fun)))
        })

        if (legend) {
            lnodes <- data.frame(label = c("Authors matching query", "Collaborators"),
                                 color = c(colors[1], colors[5]))
        }

        if (title) {
            main <- paste(
                paste0("CRAN database version<br>", format(timestamp, format = "%a, %d %b %Y, %H:%M"), collapse = ""),
                "<br>",
                if (!is.null(author)) paste0("Author names with<br> \"", paste(author, collapse = "\", \""), "\"", collapse = ""),
                "<br>",
                if (!is.null(package)) paste0("Package names with<br> \"", paste(package, collapse = "\", \""), "\"", collapse = ""))

        }


    }

    export_name <- paste0("cranly_network-", format(timestamp, format = "%d-%b-%Y"), "-", paste0(c(author, package), collapse = "-"))

    visNetwork::visNetwork(nodes_subset, edges_subset, height = height, width = width,
                           main = list(text = main,
                                       style = "font-family:Georgia, Times New Roman, Times, serif;font-size:15px")) %>%
        visNetwork::visEdges(arrows = if (perspective == "author") NULL else list(to = list(enabled = TRUE, scaleFactor = 0.5)),
                             physics = nrow(nodes_subset) < physics_threshold) %>%
            visNetwork::visOptions(highlightNearest = TRUE) %>%
            visNetwork::visLegend(addNodes = lnodes, addEdges = ledges, useGroups = FALSE) %>%
            visNetwork::visInteraction(dragNodes = dragNodes, dragView = dragView, zoomView = zoomView) %>%
            visNetwork::visExport(name = export_name, label = "PNG snapshot", style = "")
}
