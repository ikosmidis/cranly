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
#' TU/B/000082)
#'
#' }
#'
#'
NULL

#' Find packages, authors, maintainers, license, versions etc by authors, packages or names matching a specific string
#'
#' @inheritParams subset.cranly_network
#' @param name a vector of character strings with the names to be matched. If `Inf` all available names in `x` are returned. If `NULL` (default) nothing is matched
#' @param flat if `TRUE` (default) then the result is an unnamed character vector. See Value for what each function returns when `flat = FALSE`.
#'
#' @return
#'
#' The result of `package_with` and `author_with` is an unnamed vector of character strings regardless of the value  of `flat`. If `flat = TRUE` the the result of the other extractor function is as follows:
#' - `package_by`:  vector of package names named by maintainer
#' - `author_of`: list of vectors of author names named by package
#' - `suggests`:  list of vectors of package names named by package
#' - `depends`: list of vectors of package names named by package
#' - `imports`: list of vectors of package names named by package
#' - `linking_to`: list of vectors of package names named by package
#' - `maintainer_of`: vector of maintainers named by package
#' - `maintained_by`: named vector of packages
#' - `email_of`: named vector of emails
#' - `email_with`: named vector of emails
#' - `description_of`: named vector of descriptions
#' - `title_of`: named vector of titles
#' - `license_of`: named vector of license strings
#' - `version_of`: named vector of version numbers
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
#' @export
package_by <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("package_by")
}

#' @rdname package_by
#' @export
package_with <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("package_with")
}

#' @rdname package_by
#' @export
author_with <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("author_with")
}

#' @rdname package_by
#' @export
author_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("author_of")
}

#' @rdname package_by
#' @export
suggests <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("suggests")
}

#' @rdname package_by
#' @export
imports <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("imports")
}

#' @rdname package_by
#' @export
depends <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("depends")
}

#' @rdname package_by
#' @export
linking_to <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("linking_to")
}

#' @rdname package_by
#' @export
enhances <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("enhances")
}

#' @rdname package_by
#' @export
maintainer_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("maintainer_of")
}


#' @rdname package_by
#' @export
maintained_by <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("maintained_by")
}

#' @rdname package_by
#' @export
email_of <- function(x, author = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("email_of")
}

#' @rdname package_by
#' @export
email_with <- function(x, name = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("email_with")
}


#' @rdname package_by
#' @export
description_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("description_of")
}


#' @rdname package_by
#' @export
title_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("title_of")
}


#' @rdname package_by
#' @export
license_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("license_of")
}


#' @rdname package_by
#' @export
version_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("version_of")
}


#' @rdname package_by
#' @export
release_date_of <- function(x, package = NULL, exact = FALSE, flat = TRUE) {
    UseMethod("release_date_of")
}




#' `build_network` method for an object
#'
#' @param object an object to use for building a network
#' @param ... other arguments to be passed to the method
#'
#' @seealso build_network.cranly_network
#'
#' @export
build_network <- function(object, ...) {
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


