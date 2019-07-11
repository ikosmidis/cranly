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
#' [`build_network.cran_db`] [`subset.cranly_network`] [`plot.cranly_network`]
#'
#' 
#' @examples
#' \donttest{
#' # Using a package directives network
#' data("pkg_net_20190710", package = "cranly")
#' ## Find all packages containing glm in their name
#' package_with(pkg_net_20190710, name = "glm")
#' ## Find all authors of packages containing brglm in their name
#' author_of(pkg_net_20190710, package = "rglm", exact = FALSE)
#' ## Find all packages with brglm in their name
#' package_with(pkg_net_20190710, name = "rglm", exact = FALSE)
#' ## Find all authors of the package brglm2
#' author_of(pkg_net_20190710, package = "brglm2", exact = TRUE)
#' ## Find all authors with Ioannis in their name
#' author_with(pkg_net_20190710, name = "Ioannis", exact = FALSE)
#' ## Find all packages that package Rcpp suggests
#' suggests(pkg_net_20190710, package = "Rcpp", exact = TRUE)
#' ## Find all packages that package Rcpp imports
#' imports(pkg_net_20190710, package = "Rcpp", exact = TRUE)
#' ## Find all packages that package RcppArmadillo is linking to
#' linking_to(pkg_net_20190710, package = "RcppArmadillo", exact = TRUE)
#' release_date_of(pkg_net_20190710, package = "RcppArmadillo", exact = TRUE)
#' release_date_of(pkg_net_20190710, package = "brglm", exact = FALSE)
#'
#' ## Using an author collaboration network
#' data("aut_net_20190710", package = "cranly")
#' ## Find all packages containing glm in their name
#' package_with(aut_net_20190710, name = "glm")
#' ## Find all authors of packages containing brglm in their name
#' author_of(aut_net_20190710, package = "rglm", exact = FALSE)
#' ## Find all packages with brglm in their name
#' package_with(aut_net_20190710, name = "rglm", exact = FALSE)
#' ## Find all authors of the package brglm2
#' author_of(aut_net_20190710, package = "brglm2", exact = TRUE)
#' ## Find all authors with Ioannis in their name
#' author_with(aut_net_20190710, name = "Ioannis", exact = FALSE)
#' }
#'
#' 
#' @name extractor-functions
#' @aliases package_by package_with author_with author_of suggests imports depends linking_to enhances maintainer_of maintained_by email_of email_with description_of title_of license_of version_of release_date_of 
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
suggests.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
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
imports.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
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
depends.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
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
        out <- unique(unlist(x$nodes[inds, "linkingto"]))
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
enhances.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
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
        out <- unique(unlist(x$nodes[inds, "description"]))
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
        out <- unique(unlist(x$nodes[inds, "title"]))
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
        out <- unique(unlist(x$nodes[inds, "license"]))
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
        print(x$nodes[inds, "package"])
        out <- unique(unlist(x$nodes[inds, "version"]))
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
        out <- unique(unlist(x$nodes[inds, "published"]))
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

