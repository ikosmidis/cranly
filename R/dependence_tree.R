# Copyright (C) 2018- Ioannis Kosmidis

#' Computes the dependence tree of a package
#'
#' @inheritParams extractor-functions
#' @param generation integer. The original generation for the package.
#'
#' @seealso [`build_dependence_tree.cranly_network`]
#'
#' @details
#'
#' Implements a recursion that computes the full dependence tree of a
#' `package` from `x`. Specifically, the packages that are
#' requirements for `package` (`Depends`, `Imports` or
#' `LinkingTo`) are found, then the requirements for those
#' packages are found, and so on.
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

#' Construct a [`cranly_dependence_tree`] object
#'
#' @aliases cranly_dependence_tree
#' @inheritParams  plot.cranly_network
#'
#' 
#' @seealso [`compute_dependence_tree`] [`plot.cranly_dependence_tree`] [`summary.cranly_dependence_tree`]
#' 
#' @examples
#' \donttest{
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
                directive = c("imports", "depends", "linking_to"),
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

#' summary method for [`cranly_dependence_tree`] objects
#'
#' Hard dependence summaries for R packages from a [`cranly_dependence_tree`] object
#'
#' @param object a [`cranly_dependence_tree`] object.
#' @param ... currently not used.
#'
#' @return
#'
#' A list with components `n_generations`, `parents`, and `dependence_index`.
#'
#' @details
#'
#' The summary method for a [`cranly_dependence_tree`] object returns
#' the number of generations the R package(s) in the object inherit
#' from (`n_generations`), the immediate parents of the R package(s)
#' (`parents`), and a dependence index `dependence_index` defined as
#' \deqn{ -\frac{\sum_{i \in C_p; i \ne p} \frac{1}{N_i} g_i}{\sum_{i
#' \in C_p; i \ne p} \frac{1}{N_i}} }
#'
#' where \eqn{C_p} is the dependence tree for the package(s) \eqn{p},
#' \eqn{N_i} is the total number of packages that depend, link or
#' import package \eqn{i}, and \eqn{g_i} is the generation that
#' package \eqn{i} appears in the dependence tree of package(s)
#' \eqn{p}. The generation takes values on the non-positive integers,
#' with the package(s) \eqn{p} being placed at generation `0`, the
#' packages that \eqn{p} links to, depends or imports at generation
#' `-1` and so on.
#'
#' A dependence index of zero means that the \eqn{p} only has
#' immediate parents. The dependence index weights the dependencies
#' based on how popular these are, in the sense that the index is
#' not penalised if the package depends on popular packages. The
#' greatest the dependence index is the more baggage the package
#' carries, and the maintainers may want to remove any dependencies
#' that are not necessary.
#'
#' @seealso [`build_dependence_tree.cranly_network`] [`compute_dependence_tree`]
#'
#' @examples
#' \donttest{
#' cran_db <- clean_CRAN_db()
#' package_network <- build_network(object = cran_db)
#'
#' ## Two light packages
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
    summ <- object$summaries
    if (nrow(summ)) {
        w <- with(object, 1/rowSums(summaries[c("n_imported_by", "n_depended_by", "n_linked_by")]))
        g <- object$nodes$generation
        ind <- names(w) != object$package
        dependence_index <- weighted.mean(-g[ind], w[ind]) - 1
        n_generations <- -min(object$nodes$generation)
        parents <- with(object, nodes$package[nodes$generation == -1])
    }
    else {
        n_generations <- 0
        dependence_index <- 0
        parents <- NULL
    }
    list(package = object$package,
         n_generations = n_generations,
         parents = parents,
         dependence_index = dependence_index)
}
