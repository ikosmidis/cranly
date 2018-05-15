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
        str <- paste(author, collapse = "$|^")
        str <- paste0("^", str, "$")
    }
    else {
        str <- paste(author, collapse = "|")
    }
    inds <- sapply(x$nodes$author, function(z) any(grepl(str, z, ignore.case = !exact)))
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
    ## Escape .
    name <- gsub("\\.", "\\\\.", name)
    if (exact) {
        str <- paste(name, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(name, collapse = "|")
    }
    package <- unlist(x$nodes$package)
    inds <- grep(str, package, ignore.case = !exact, perl = TRUE)
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
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
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
        str <- paste(name, collapse = "$|^")
        str <- paste0("^", str, "$")
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
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
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
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
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
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    ## inds <- sapply(x$nodes$Package, function(x) any(grepl(str, x)))
    out <- unique(unlist(x$nodes[inds, "linkingto"]))
    if (length(out)) {
        out
    }
    else {
        NULL
    }
}

