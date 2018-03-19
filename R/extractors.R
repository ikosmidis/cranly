#' @export
package_by.cranly_network <- function(object, author = NULL, exact = FALSE, ...) {
    if (is.null(author)) {
        return(NULL) # return(unlist(object$nodes$Package))
    }
    perspective <- attr(object, "perspective")
    str <- paste(author, collapse = "|")
    if (exact) str <- paste0("\\b", str, "\\b")
    inds <- grep(str, object$nodes$Author, ignore.case = !exact)
    unlist(object$nodes[inds, "Package"])
}

#' @export
author_of.cranly_network <- function(object, package = NULL, exact = FALSE, ...) {
    if (is.null(package)) {
        return(NULL) # return(unlist(object$nodes$Package))
    }
    perspective <- attr(object, "perspective")
    str <- paste(package, collapse = "|")
    if (exact) str <- paste0("\\b", str, "\\b")
    inds <- grep(str, object$nodes$Package, ignore.case = !exact)
    ## inds <- sapply(object$nodes$Package, function(x) any(grepl(str, x)))
    unlist(object$nodes[inds, "Author"])
}

#' @export
author_with.cranly_network <- function(object, name = NULL, exact = FALSE) {
    if (is.null(name)) {
        return(NULL) #return(unlist(object$nodes$Author))
    }
    perspective <- attr(object, "perspective")
    str <- paste(name, collapse = "|")
    if (exact) str <- paste0("\\b", str, "\\b")
    authors <- unlist(object$nodes$Author)
    inds <- grep(str, authors, ignore.case = !exact)
    unique(authors[inds])
}

#' @export
package_with.cranly_network <- function(object, name = NULL, exact = FALSE) {
    if (is.null(name)) {
        return(NULL) #return(unlist(object$nodes$Author))
    }
    perspective <- attr(object, "perspective")
    str <- paste(name, collapse = "|")
    if (exact) str <- paste0("\\b", str, "\\b")
    package <- unlist(object$nodes$Package)
    inds <- grep(str, package, ignore.case = !exact)
    unique(package[inds])
}

