# Copyright (C) 2018 Ioannis Kosmidis

#' Computes the dependence tree of a package
#'
#' @inheritParams package_by
#' @param generation integer. The original generation for the package.
#' @seealso build_dependence_tree
#'
#' @details
#'
#' Implements a recursion that computes the full dependende tree of a
#' \code{package} from \code{x}. Specifically, the packages that are
#' requirements for \code{package} (\code{Depends}, \code{Imports} or
#' \code{LinkingTo}) are found, then the requirements for those
#' packages, and so on.
#'
#' @export
compute_dependence_tree <- function(x, package = NULL, generation = 0) {
    if (is.null(package)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    im <- imports(x, package = package, exact = TRUE)
    de <- depends(x, package = package, exact = TRUE)
    li <- linking_to(x, package = package, exact = TRUE)
    pack <- na.omit(c(im, de, li))
    if (all(pack %in% package)) {
        data.frame(package = unique(package), generation = generation, stringsAsFactors = FALSE)
    }
    else {
        rbind(data.frame(package = unique(package), generation = generation, stringsAsFactors = FALSE),
              compute_dependence_tree(x, package = pack, generation - 1))
    }
}

#' Construct a \code{\link{cranly_dependence_tree}} object
#'
#' @aliases cranly_dependence_tree
#' @inheritParams  plot.cranly_network
#' @seealso compute_dependence_tree
#'
#' @examples
#' \dontrun{
#' cran_db <- clean_CRAN_db()
#' package_network <- build_network(object = cran_db)
#' dep_tree <- build_dependence_tree(package_network, package = "PlackettLuce")
#' plot(dep_tree)
#' }
#'
#' @export
build_dependence_tree.cranly_network <- function(x,
                                                 package = Inf,
                                                 base = FALSE,
                                                 recommended = TRUE,
                                                 global = TRUE,
                                                 ...) {

    if (any(is.infinite(package))) {
        stop("Please specify package")
    }

    if (global) {
        summaries <- summary(x, advanced = FALSE)
    }

    d_tree <- compute_dependence_tree(x, package = package, generation = 0)
    pack0 <- package
    package <- unique(d_tree$package)

    x <- subset(x, package = package, author = Inf,
                directive = c("imports", "depends", "linkingto"),
                exact = TRUE, only = TRUE,
                base = base, recommended = recommended)

    ## Make sure that nodes has only what is in edges
    ## x$nodes <- subset(x$nodes, package %in% c(x$edges$from, x$edges$to))
    x$nodes <- subset(x$nodes, package %in% c(x$edges$from, x$edges$to))

    d_tree <- d_tree[d_tree$package %in% x$nodes$package, ]

    if (!global) {
        summaries <- summary(x, advanced = FALSE)
    }

    ## First matches so earliest generation if a package appears twice
    x$nodes$generation <- d_tree$generation[match(x$nodes$package, d_tree$package)]
    x$summaries <- summaries[x$nodes$package, ]
    x$package <- pack0

    class(x) <- c("cranly_dependence_tree", class(x))
    x
}

#' Hard-dependence summaries for R packages from a \code{\link{cranly_dependence_tree}} object
#'
#' @param object a \code{\link{cranly_dependence_tree}} object
#' @param ... currently not used
#'
#' @return
#'
#' A list with components \code{n_generations}, \code{parents}, and
#' \code{dependence_index}.
#'
#' @details
#'
#' The summary method for a \code{\link{cranly_dependence_tree}}
#' object returns the number of generations the R package(s) in the
#' object inherit from (\code{n_generations}), the immediate
#' parents of the R package(s) (\code{parents}), and a dependence
#' index \code{dependence_index} defined as
#' \deqn{
#' \frac{\sum_{i \in C_p; i \ne p} \frac{1}{N_i} g_i}{\sum_{i \in C_p; i \ne p} \frac{1}{N_i}}
#' }
#'
#' where \eqn{C_p} is the dependence tree for the package(s) \eqn{p},
#' \eqn{N_i} is the total number of packages that depend, link or
#' import package \eqn{i}, and \eqn{g_i} is the generation that
#' package \eqn{i} appears in the dependence tree of package(s)
#' \eqn{p}. The generation takes values on the non-positive integers,
#' with the package(s) \eqn{p} being at the \code{0} generation, the
#' packages that \eqn{p} links to, depends or imports are generation
#' \code{-1} and so on.
#'
#' A dependence index of zero means that the \eqn{p} only has
#' immediate parents. The dependence index weights the dependencies
#' based on how "popular" these are, in the sense that the index is
#' not penalised if the package depends on popular packages. The
#' greatest the dependence index is the more baggage the package
#' carries, and the maintainers may want to remove any dependencies
#' that are not necessary.
#'
#' @seealso build_dependence_tree compute_dependence_tree
#'
#' @examples
#' \dontrun{
#' cran_db <- clean_CRAN_db()
#' package_network <- build_network(object = cran_db)
#'
#' ## Two "light" packages
#' dep_tree <- build_dependence_tree(package_network, package = "brglm")
#' summary(dep_tree)
#'
#' dep_tree <- build_dependence_tree(package_network, package = "gnm")
#' summary(dep_tree)
#'
#' ## A somewhat heavier package (sorry)...
#' dep_tree <- build_dependence_tree(package_network, package = "cranly")
#' summary(dep_tree)
#'
#' }
#'
#' @export
summary.cranly_dependence_tree <- function(object, ...) {
    w <- with(object, 1/rowSums(summaries[c("n_imported_by", "n_depended_by", "n_linked_by")]))
    g <- object$nodes$generation
    ind <- names(w) != object$package
    dependence_index <- weighted.mean(-g[ind], w[ind]) - 1
    list(n_generations = -min(object$nodes$generation),
         parents = with(object, nodes$package[nodes$generation == -1]),
         dependence_index = dependence_index)
}

#' Interactive visualization of a package's dependence tree from a \code{\link{cranly_network}}
#'
#' @inheritParams plot.cranly_network
#' @param x a \code{\link{cranly_dependence_tree}} object
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
        print(res %>% visNetwork::visHierarchicalLayout(levelSeparation = 50))
    }
    invisible(res)
}


