# Copyright (C) 2018 Ioannis Kosmidis


#' Find packages, authors, maintainers, license, versions etc by authors, packages or names matching a specific string
#'
#'
#' @inheritParams subset.cranly_network
#' @param package a vector of character strings with the package names to be matched. If `Inf` all available packages in `x` are returned. If `NULL` (default) nothing is matched.
#' @param author a vector of character strings with the author names to be matched. If `Inf` all available authors in `x` are returned. If `NULL` (default) nothing is matched.
#' @param name a vector of character strings with the names to be matched. If `Inf` all available names in `x` are returned. If `NULL` (default) nothing is matched.
#' @param flat if `TRUE` (default) then the result is an unnamed character vector. See Value for more details of what is returned.
#' @param exact logical. Should we use exact matching? Default is `TRUE`.
#'
#' @details
#'
#' The extractor functions all try to figure out what `y` is in the statement
#'
#' `y` is (the) `extractor-function` a `package`/`author`.
#'
#' For example, for
#'
#' - "`y` is the package by `"Kurt Hornik"`" we do `package_by(x, "Kurt Hornik")`
#'
#' - "`y` is the author of a package with a name matching `"MASS"`" we do `author_of(x, "MASS")`
#'
#' - "`y` is the package enhanced by the `"prediction"` package we do `enhanced_by(x, "prediction", exact = TRUE)`
#'
#' - "`y` is the package linking to `"Rcpp"` we do `linking_to(x, "Rcpp", exact = TRUE)`
#'
#' @return
#'
#' If `flat = TRUE` then the result of the extraction function is a
#' `data.frame`, which is the subset of `x$nodes` matching `author`,
#' `name` or `package` (according to the value of `exact`). If `flat =
#' FALSE` then the results is a vector.
#'
#' When `flat = TRUE` any [`NA`]s are removed before the result is
#' returned.
#'
#' @seealso
#' [build_network.cranly_db()] [subset.cranly_network()] [plot.cranly_network()]
#'
#'
#' @examples
#' \donttest{
#' # Using a package directives network
#' cran_db <- clean_CRAN_db()
#' package_network <- build_network(cran_db)
#'
#' ## Find all packages containing glm in their name
#' package_with(package_network, name = "glm")
#' ## Find all authors of packages containing brglm in their name
#' author_of(package_network, package = "rglm", exact = FALSE)
#' ## Find all packages with brglm in their name
#' package_with(package_network, name = "rglm", exact = FALSE)
#' ## Find all authors of the package brglm2
#' author_of(package_network, package = "brglm2", exact = TRUE)
#' ## Find all authors with Ioannis in their name
#' author_with(package_network, name = "Ioannis", exact = FALSE)
#' ## Find all packages suggested by Rcpp
#' suggested_by(package_network, package = "Rcpp", exact = TRUE)
#' ## Find all packages imported by Rcpp
#' imported_by(package_network, package = "Rcpp", exact = TRUE)
#' ## Find all packages enhacing brglm
#' enhancing(package_network, package = "brglm", exact = TRUE)
#' ## Find all packages linking to RcppArmadillo
#' linking_to(package_network, package = "RcppArmadillo", exact = TRUE)
#' ## Find the release date of RcppArmadillo
#' release_date_of(package_network, package = "RcppArmadillo", exact = TRUE)
#' ## Find the release data of all packages with "brglm" in their name
#' release_date_of(package_network, package = "brglm", exact = FALSE)
#' ## More information about packages with "brglm" in their name
#' release_date_of(package_network, package = "brglm", exact = FALSE,
#'                 flat = FALSE)[c("package", "version")]
#'
#' ## Using an author collaboration network
#' data("author_network", package = "cranly")
#' ## Find all packages containing glm in their name
#' package_with(author_network, name = "glm")
#' ## Find all authors of packages containing brglm in their name
#' author_of(author_network, package = "rglm", exact = FALSE)
#' ## Find all packages with brglm in their name
#' package_with(author_network, name = "rglm", exact = FALSE)
#' ## Find all authors of the package brglm2
#' author_of(author_network, package = "brglm2", exact = TRUE)
#' ## Find all authors with Ioannis in their name
#' author_with(author_network, name = "Ioannis", exact = FALSE)
#' }
#'
#'
#' @name extractor-functions
#' @aliases package_by package_with author_with author_of suggested_by imported_by dependency_of linked_by enhanced_by suggesting importing depending_on linking_to enhancing maintainer_of maintained_by email_of email_with description_of title_of license_of version_of release_date_of
NULL


## The output is out[!is.na(out)] to ignore outputs that we have no
## information about, e.g. packages from bioconductor

#' @rdname extractor-functions
#' @export
package_by.cranly_network <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(author)) {
        return(character(0)) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(author))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        if (exact) {
            str <- paste(author, collapse = "$|^")
            str <- paste0("^", str, "$")
        }
        else {
            str <- paste(author, collapse = "|")
        }
        inds <- sapply(x$nodes$author, function(z) any(grepl(str, z, ignore.case = !exact)))
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "package"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
package_with.cranly_network <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(name)) {
        return(character(0)) #return(unlist(x$nodes$Author))
    }
    if (any(is.infinite(name))) {
        inds <- inds_row <- rep(TRUE, nrow(x$nodes))
    }
    else {
        name <- gsub("\\.", "\\\\.", name)
        if (exact) {
            str <- paste(name, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(name, collapse = "|")
        }
        inds <- sapply(x$nodes$package, function(z) grepl(str, z, ignore.case = !exact, perl = TRUE))
        inds_row <- sapply(inds, any)
    }

    if (flat) {
        out <- x$nodes[inds_row, "package"]
        inds <- inds[inds_row]
        out <- unique(unlist(lapply(seq.int(out), function(j) out[[j]][inds[[j]]])))

        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds_row, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
author_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "author"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
author_with.cranly_network <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(name)) {
        return(character(0))
    }
    if (any(is.infinite(name))) {
        inds <- inds_row <- rep(TRUE, nrow(x$nodes))
    }
    else {
        if (exact) {
            str <- paste(name, collapse = "$|^")
            str <- paste0("^", str, "$")
        }
        else {
            str <- paste(name, collapse = "|")
        }
        inds <- sapply(x$nodes$author, function(z) grepl(str, z, ignore.case = !exact))
        inds_row <- sapply(inds, any)
    }
    if (flat) {
        out <- x$nodes[inds_row, "author"]
        inds <- inds[inds_row]
        out <- unique(unlist(lapply(seq.int(out), function(j) out[[j]][inds[[j]]])))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds_row, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
suggested_by.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "suggests"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}


#' @rdname extractor-functions
#' @export
suggesting.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "reverse_suggests"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}



#' @rdname extractor-functions
#' @export
imported_by.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "imports"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
importing.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "reverse_imports"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}


#' @rdname extractor-functions
#' @export
dependency_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "depends"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}


#' @rdname extractor-functions
#' @export
depending_on.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "reverse_depends"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}


#' @rdname extractor-functions
#' @export
linked_by.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "linking_to"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
linking_to.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "reverse_linking_to"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}


#' @rdname extractor-functions
#' @export
enhanced_by.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "enhances"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}



#' @rdname extractor-functions
#' @export
enhancing.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "reverse_enhances"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}




#' @rdname extractor-functions
#' @export
maintainer_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "maintainer"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
maintained_by.cranly_network <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(author)) {
        return(character(0))
    }
    if (any(is.infinite(author))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        if (exact) {
            str <- paste(author, collapse = "$|^")
            str <- paste0("^", str, "$")
        }
        else {
            str <- paste(author, collapse = "|")
        }
        inds <- sapply(x$nodes$maintainer, function(z) any(grepl(str, z, ignore.case = !exact)))
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "package"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
email_of.cranly_network <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(author)) {
        return(character(0))
    }
    if (any(is.infinite(author))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        if (exact) {
            str <- paste(author, collapse = "$|^")
            str <- paste0("^", str, "$")
        }
        else {
            str <- paste(author, collapse = "|")
        }
        inds <- sapply(x$nodes$maintainer, function(z) any(grepl(str, z, ignore.case = !exact)))
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "email"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
email_with.cranly_network <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(name)) {
        return(character(0))
    }
    if (any(is.infinite(name))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        ## Escape .
        name <- gsub("\\.", "\\\\.", name)
        if (exact) {
            str <- paste(name, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(name, collapse = "|")
        }
        inds <- sapply(x$nodes$email, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    }
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "email"]))
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
description_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unlist(x$nodes[inds, "description"])
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
title_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unlist(x$nodes[inds, "title"])
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
license_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unlist(x$nodes[inds, "license"])
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
version_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unlist(x$nodes[inds, "version"])
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

#' @rdname extractor-functions
#' @export
release_date_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        inds <- rep(TRUE, nrow(x$nodes))
    }
    else {
        package <- gsub("\\.", "\\\\.", package)
        if (exact) {
            str <- paste(package, collapse = "$(?!\\.)|^")
            str <- paste0("^", str, "$(?!\\.)")
        }
        else {
            str <- paste(package, collapse = "|")
        }
        inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    }
    if (flat) {
        out <- unlist(x$nodes[inds, "published"])
        if (all(is.na(out)) | !length(out)) {
            out <- character(0)
        }
        else {
            out <- out[!is.na(out)]
        }
    }
    else {
        out <- x$nodes[inds, ]
    }
    out
}

