# Copyright (C) 2018 Ioannis Kosmidis

#' @rdname package_by
#' @export
package_by.cranly_network <- function(x, author = NULL, exact = FALSE) {
    if (is.null(author)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(author))) {
        return(unique(unlist(x$nodes$package)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(author, collapse = "\\b$|^\\b")
        str <- paste0("^\\b", str, "\\b$")
    }
    else {
        str <- paste(author, collapse = "|")
    }
    inds <- grep(str, x$nodes$author, ignore.case = !exact)
    out <- unique(unlist(x$nodes[inds, "package"]))
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}

#' @rdname package_by
#' @export
package_with.cranly_network <- function(x, name = NULL, exact = FALSE) {
    if (is.null(name)) {
        return(NULL) #return(unlist(x$nodes$Author))
    }
    if (any(is.infinite(name))) {
        return(unique(unlist(x$nodes$package)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(name, collapse = "\\b$|^\\b")
        str <- paste0("^\\b", str, "\\b$")
    }
    else {
        str <- paste(name, collapse = "|")
    }
    package <- unlist(x$nodes$package)
    inds <- grep(str, package, ignore.case = !exact)
    out <- unique(package[inds])
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}

#' @rdname package_by
#' @export
author_of.cranly_network <- function(x, package = NULL, exact = FALSE) {
    if (is.null(package)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$author)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(package, collapse = "\\b|\\b")
        str <- paste0("\\b", str, "\\b")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact)
    out <- unique(unlist(x$nodes[inds, "author"]))
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}

#' @rdname package_by
#' @export
author_with.cranly_network <- function(x, name = NULL, exact = FALSE) {
    if (is.null(name)) {
        return(NULL) #return(unlist(x$nodes$Author))
    }
    if (any(is.infinite(name))) {
        return(unique(unlist(x$nodes$author)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(name, collapse = "\\b$|^\\b")
        str <- paste0("^\\b", str, "\\b$")
    }
    else {
        str <- paste(name, collapse = "|")
    }
    authors <- unlist(x$nodes$author)
    inds <- grep(str, authors, ignore.case = !exact)
    out <- unique(authors[inds])
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}

#' @rdname package_by
#' @export
imports <- function(x, package = NULL, exact = FALSE) {
    if (is.null(package)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$imports)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(package, collapse = "\\b$|^\\b")
        str <- paste0("^\\b", str, "\\b$")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact)
    ## inds <- sapply(x$nodes$Package, function(x) any(grepl(str, x)))
    out <- unique(unlist(x$nodes[inds, "imports"]))
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}

#' @rdname package_by
#' @export
depends <- function(x, package = NULL, exact = FALSE) {
    if (is.null(package)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$depends)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(package, collapse = "\\b$|^\\b")
        str <- paste0("^\\b", str, "\\b$")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact)
    ## inds <- sapply(x$nodes$Package, function(x) any(grepl(str, x)))
    out <- unique(unlist(x$nodes[inds, "depends"]))
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}


#' @rdname package_by
#' @export
linking_to <- function(x, package = NULL, exact = FALSE) {
    if (is.null(package)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$linkingto)))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(package, collapse = "\\b$|^\\b")
        str <- paste0("^\\b", str, "\\b$")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact)
    ## inds <- sapply(x$nodes$Package, function(x) any(grepl(str, x)))
    out <- unique(unlist(x$nodes[inds, "linkingto"]))
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}


#' Computes the dependence tree of a package
#'
#' @inheritParams package_by
#' @param generation integer. The original generation for the package.
#'
#' @details
#'
#' Implements a recursion that computes the full dependende tree of a
#' \code{package} from \code{x}. Specifically, the packages that are
#' requirements for \code{package} (\code{Depends}, \code{Imports} or
#' \code{LinkingTo}) are found, then the requirements for those
#' packages, and so on.
#'
#' @examples
#' \dontrun{
#' data("cran_sample", package = "cranly")
#' package_network <- build_network(object = cran_sample)
#' dependence_tree(package_network, package = "PlackettLuce")
#' }
#'
#' @export
dependence_tree <- function(x, package = NULL, generation = 0) {
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
              dependence_tree(x, package = pack, generation - 1))
    }
}



## dependence_tree <- function(x, package = NULL) {
##     im <- imports(x, package = package, exact = TRUE)
##     de <- depends(x, package = package, exact = TRUE)
##     li <- linking_to(x, package = package, exact = TRUE)
##     pack <- na.omit(c(im, de, li))
##     if (all(pack %in% package)) {
##         package
##     }
##     else {
##         unique(c(package, dependence_tree(x, package = pack)))
##     }
## }
