# Copyright (C) 2018 Ioannis Kosmidis

#' cranly: CRAN package database analytics and visualizations
#'
#' @docType package
#' @name cranly
#' @import igraph
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_split
#' @importFrom utils combn stack head installed.packages
#' @importFrom stats na.omit
#' @import countrycode
#' @importFrom ggplot2 ggplot geom_bar theme_minimal labs coord_flip
#' @importFrom stats weighted.mean
#'
#' @details
#'
#' \pkg{cranly} provides core visualisations and summaries for the
#' CRAN package database. The package provides comprehensive methods
#' for cleaning up and organising the information in the CRAN package
#' database, for building package directives networks (depends,
#' imports, suggests, enhances, linking to) and collaboration
#' networks, and for computing summaries and producing interactive
#' visualisations from the resulting networks. Network visualisation
#' is through the \pkg{visNetwork}
#' (<https://CRAN.R-project.org/package=visNetwork>) package. The
#' package also provides functions to coerce the networks to
#' \pkg{igraph} <https://CRAN.R-project.org/package=igraph>
#' objects for further analyses and modelling.
#'
#' @section Acknowledgements:
#' \itemize{
#'
#' \item David Selby (<http://selbydavid.com>) experimented with
#' and provided helpful comments and feedback on a pre-release version
#' of \pkg{cranly}. His help is gratefully acknowledged.
#'
#' \item This work has been partially supported by the Alan Turing
#' Institute under the EPSRC grant EP/N510129/1 (Turing award number
#' TU/B/000082).
#'
#' }
#'
#'
NULL

#' @export
package_by <- function(x, author, exact, flat) {
    UseMethod("package_by")
}

#' @export
package_with <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("package_with")
}

#' @export
author_with <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("author_with")
}

#' @export
author_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("author_of")
}

#' @export
suggests <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("suggests")
}

#' @export
imports <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("imports")
}

#' @export
depends <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("depends")
}

#' @export
linking_to <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("linking_to")
}

#' @export
enhances <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("enhances")
}

#' @export
maintainer_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("maintainer_of")
}

#' @export
maintained_by <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("maintained_by")
}

#' @export
email_of <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("email_of")
}

#' @export
email_with <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("email_with")
}

#' @export
description_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("description_of")
}

#' @export
title_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("title_of")
}

#' @export
license_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("license_of")
}

#' @export
version_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("version_of")
}

#' @export
release_date_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("release_date_of")
}

#' @export
build_network <- function(object, trace, perspective, ...) {
    UseMethod("build_network")
}

#' `build_dependence_tree` method for an object
#'
#' @param x an object to use for building a dependence tree
#' @param ... other arguments to be passed to the method
#'
#' @seealso build_network.cranly_network compute_dependence_tree
#'
#' @export
build_dependence_tree <- function(x, ...) {
    UseMethod("build_dependence_tree")
}

if(getRversion() >= "2.15.1")  {
    utils::globalVariables(c("author", "package", "version", "from", "n_depended_by", "n_depends", "n_enhanced_by", "n_enhances", "n_imported_by", "n_imports", "n_suggested_by", "n_suggests", "to", "type", "n_collaborators", "maintainer", "n_linking_to", "n_linked_by", "generation", "priority"))
}


