#' @export
package_by.cranly_network <- function(object, author = NULL) {
    if (is.null(author)) {
        return(unlist(object$nodes$Package))
    }
    perspective <- attr(object, "perspective")
    str <- paste(author, collapse = "|")
    if (perspective == "package") {
        inds <- sapply(object$nodes$Author, function(x) any(grepl(str, x)))
    }
    else {
        inds <- grep(str, object$nodes$Author)
    }
    unlist(object$nodes[inds, "Package"])
}

#' @export
author_of.cranly_network <- function(object, package = NULL) {
    if (is.null(package)) {
        return(unlist(object$nodes$Package))
    }
    perspective <- attr(object, "perspective")
    str <- paste(package, collapse = "|")
    if (perspective == "package") {
        inds <- grep(str, object$nodes$Package)
    }
    else {
        inds <- sapply(object$nodes$Package, function(x) any(grepl(str, x)))
    }
    unlist(object$nodes[inds, "Author"])

}
