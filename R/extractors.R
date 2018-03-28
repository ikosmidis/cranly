# Copyright (C) 2018 Ioannis Kosmidis

#' @rdname package_by
#' @export
package_by.cranly_network <- function(x, author = NULL, exact = FALSE) {
    if (is.null(author)) {
        return(NULL) # return(unlist(x$nodes$Package))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(author, collapse = "\\b|\\b")
        str <- paste0("\\b", str, "\\b")
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
author_of.cranly_network <- function(x, package = NULL, exact = FALSE) {
    if (is.null(package)) {
        return(NULL) # return(unlist(x$nodes$Package))
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
    ## inds <- sapply(x$nodes$Package, function(x) any(grepl(str, x)))
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
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(name, collapse = "\\b|\\b")
        str <- paste0("\\b", str, "\\b")
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
package_with.cranly_network <- function(x, name = NULL, exact = FALSE) {
    if (is.null(name)) {
        return(NULL) #return(unlist(x$nodes$Author))
    }
    perspective <- attr(x, "perspective")
    if (exact) {
        str <- paste(name, collapse = "\\b|\\b")
        str <- paste0("\\b", str, "\\b")
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

