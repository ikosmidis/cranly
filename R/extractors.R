# Copyright (C) 2018 Ioannis Kosmidis

## The output is out[!is.na(out)] to ignore outputs that we have no
## information about, e.g. packages from bioconductor

#' @export
package_by.cranly_network <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(author)) {
        return(character(0)) # return(unlist(x$nodes$Package))
    }
    if (any(is.infinite(author))) {
        return(unique(unlist(x$nodes$package)))
    }
    if (exact) {
        str <- paste(author, collapse = "$|^")
        str <- paste0("^", str, "$")
    }
    else {
        str <- paste(author, collapse = "|")
    }
    inds <- sapply(x$nodes$author, function(z) any(grepl(str, z, ignore.case = !exact)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "package"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "maintainer")])
        out <- structure(out$package, names = out$maintainer)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
package_with.cranly_network <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(name)) {
        return(character(0)) #return(unlist(x$nodes$Author))
    }
    if (any(is.infinite(name))) {
        return(unique(unlist(x$nodes$package)))
    }
    name <- gsub("\\.", "\\\\.", name)
    if (exact) {
        str <- paste(name, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(name, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "package"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "maintainer")])
        out <- structure(out$package, names = out$maintainer)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
author_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$author)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "author"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "author")])
        out <- structure(out$author, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
author_with.cranly_network <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    if (is.null(name)) {
        return(character(0))
    }
    if (any(is.infinite(name))) {
        return(unique(unlist(x$nodes$author)))
    }
    if (exact) {
        str <- paste(name, collapse = "$|^")
        str <- paste0("^", str, "$")
    }
    else {
        str <- paste(name, collapse = "|")
    }
    authors <- unlist(x$nodes$author)
    inds <- sapply(authors, function(z) any(grepl(str, z, ignore.case = !exact)))
    out <- unique(authors[inds])
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
suggests.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$suggests)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "suggests"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "suggests")])
        out <- structure(out$suggests, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}



#' @export
imports.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$imports)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "imports"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "imports")])
        out <- structure(out$imports, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
depends.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$depends)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "depends"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "depends")])
        out <- structure(out$depends, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}

## `package`'s linking to
#' @export
linking_to.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$linkingto)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "linkingto"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "linkingto")])
        out <- structure(out$linkingto, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}

## `package`'s enhances
#' @export
enhances.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$enhances)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- grep(str, x$nodes$package, ignore.case = !exact, perl = TRUE)
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "enhances"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "enhances")])
        out <- structure(out$enhances, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
maintainer_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$maintainer)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "maintainer"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "maintainer")])
        out <- structure(out$maintainer, names = out$package)        
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
maintained_by.cranly_network <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(author)) {
        return(character(0))
    }
    if (any(is.infinite(author))) {
        return(unique(unlist(x$nodes$package)))
    }
    if (exact) {
        str <- paste(author, collapse = "$|^")
        str <- paste0("^", str, "$")
    }
    else {
        str <- paste(author, collapse = "|")
    }
    inds <- sapply(x$nodes$maintainer, function(z) any(grepl(str, z, ignore.case = !exact)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "package"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "maintainer")])        
        out <- structure(out$package, names = out$maintainer)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}

#'@export
email_of.cranly_network <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(author)) {
        return(character(0))
    }
    if (any(is.infinite(author))) {
        return(unique(unlist(x$nodes$maintainer)))
    }
    if (exact) {
        str <- paste(author, collapse = "$|^")
        str <- paste0("^", str, "$")
    }
    else {
        str <- paste(author, collapse = "|")
    }
    inds <- sapply(x$nodes$maintainer, function(z) any(grepl(str, z, ignore.case = !exact)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "email"]))
    }
    else {
        out <- unique(x$nodes[inds, c("maintainer", "email")])
        out <- structure(out$email, names = out$maintainer)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}

#'@export
email_with.cranly_network <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(name)) {
        return(character(0))
    }
    if (any(is.infinite(name))) {
        return(unique(unlist(x$nodes$email)))
    }
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
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "email"]))
    }
    else {
        out <- unique(x$nodes[inds, c("maintainer", "email")])
        out <- structure(out$email, names = out$maintainer)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
description_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$description)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "description"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "description")])
        out <- structure(out$description, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
title_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$title)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "title"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "title")])
        out <- structure(out$title, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
license_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$license)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "license"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "license")])
        out <- structure(out$license, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}

#' @export
version_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unique(unlist(x$nodes$version)))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unique(unlist(x$nodes[inds, "version"]))
    }
    else {
        out <- unique(x$nodes[inds, c("package", "version")])
        out <- structure(out$version, names = out$package)
    }
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}


#' @export
release_date_of.cranly_network <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    if (attr(x, "perspective") == "author") {
        stop(match.call()[[1]], " is designed for cranly_network objects with perspective = 'package'")
    }
    if (is.null(package)) {
        return(character(0))
    }
    if (any(is.infinite(package))) {
        return(unlist(x$nodes$published))
    }
    package <- gsub("\\.", "\\\\.", package)
    if (exact) {
        str <- paste(package, collapse = "$(?!\\.)|^")
        str <- paste0("^", str, "$(?!\\.)")
    }
    else {
        str <- paste(package, collapse = "|")
    }
    inds <- sapply(x$nodes$package, function(z) any(grepl(str, z, ignore.case = !exact, perl = TRUE)))
    if (flat) {
        out <- unlist(x$nodes[inds, "published"])
    }
    else {
        out <- x$nodes[inds, c("package", "published")]
        out <- structure(out$published, names = out$package)
    }
    
    if (all(is.na(out)) | !length(out)) {
        return(character(0))
    }
    else {
        return(out[!is.na(out)])
    }
}

