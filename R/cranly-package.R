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
#' (\url{https://CRAN.R-project.org/package=visNetwork}) package. The
#' package also provides functions to coerce the networks to
#' \pkg{igraph} \url{https://CRAN.R-project.org/package=igraph}
#' objects for further analyses and modelling.
#'
#' @section Acknowledgements:
#' \itemize{
#'
#' \item David Selby (\url{http://selbydavid.com}) experimented with
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

#' Find packages and authors by authors or packages with names matching a specific string
#'
#' @inheritParams subset.cranly_network
#' @param name a vector of character strings with the names to be matched. If \code{Inf} all available names in \code{x} are returned. If \code{NULL} (default) nothing is matched
#'
#' @examples
#' \dontrun{
#' cran_db <- clean_CRAN_db()
#' ## Using a package directives network
#' package_network <- build_network(cran_db)
#' ## Find all packages containing glm in their name
#' package_with(package_network, name = "glm")
#' ## Find all authors of packages containing brglm in their name
#' author_of(package_network, package = "rglm", exact = FALSE)
#' ## Find all packages with brglm in their name
#' package_with(package_network, name = "rglm", exact = FALSE)
#' ## Find all authors of the package brglm2
#' author_of(package_network, package = "brglm2", exact = TRUE)
#' ## Find all authors with Ioannis in their name
#' author_with(package_network, name = "Ioannis", exact = TRUE)
#' ## Find all packages that package Rcpp suggests
#' suggests(package_network, package = "Rcpp", exact = TRUE)
#' ## Find all packages that package Rcpp imports
#' imports(package_network, package = "Rcpp", exact = TRUE)
#' ## Find all packages that package RcppArmadillo is linking to
#' linking_to(package_network, package = "RcppArmadillo", exact = TRUE)
#'
#' ## Using an author collaboration network
#' author_network <- build_network(cran_db, perspective = "author")
#' ## Find all packages containing glm in their name
#' package_with(author_network, name = "glm")
#' ## Find all authors of packages containing brglm in their name
#' author_of(author_network, package = "rglm", exact = FALSE)
#' ## Find all packages with brglm in their name
#' package_with(author_network, name = "rglm", exact = FALSE)
#' ## Find all authors of the package brglm2
#' author_of(author_network, package = "brglm2", exact = TRUE)
#' ## Find all authors with Ioannis in their name
#' author_with(author_network, name = "Ioannis", exact = TRUE)
#' }
#' @export
package_by <- function(x, author = NULL, exact = FALSE) {
    UseMethod("package_by")
}

#' @rdname package_by
#' @export
package_with <- function(x, name = NULL, exact = FALSE) {
    UseMethod("package_with")
}

#' @rdname package_by
#' @export
author_with <- function(x, name = NULL, exact = FALSE) {
    UseMethod("author_with")
}

#' @rdname package_by
#' @export
author_of <- function(x, package = NULL, exact = FALSE) {
    UseMethod("author_of")
}

#' @rdname package_by
#' @export
suggests <- function(x, package = NULL, exact = FALSE) {
    UseMethod("suggests")
}

#' @rdname package_by
#' @export
imports <- function(x, package = NULL, exact = FALSE) {
    UseMethod("imports")
}

#' @rdname package_by
#' @export
depends <- function(x, package = NULL, exact = FALSE) {
    UseMethod("depends")
}

#' @rdname package_by
#' @export
linking_to <- function(x, package = NULL, exact = FALSE) {
    UseMethod("linking_to")
}



#' \code{build_network} method for an object
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

#' \code{build_dependence_tree} method for an object
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


