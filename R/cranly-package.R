#' cranly: CRAN package database analytics and visualizations
#'
#' @docType package
#' @name cranly
#' @import igraph
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_split
#' @importFrom utils combn stack head
#' @import countrycode
#' @importFrom ggplot2 ggplot geom_bar theme_minimal labs coord_flip
#'
NULL

#' Find packages and authors by authors or packages with names matching a specific string
#'
#' @inheritParams subset.cranly_network
#' @param name a vector of character strings with the names to be matched
#'
#' @examples
#' \dontrun{
#' data("cran20032018", package = "cranly")
#' ## Using a package directives network
#' package_network <- build_network(cran20032018)
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
#'
#' ## Using an author collaboration network
#' author_network <- build_network(cran20032018, perspective = "author")
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

#' \code{visualize} method for an object
#'
#' @param object an object to be visualized
#' @param ... other arguments to be passed to the method
#'
#' @seealso visualize.cranly_network
#'
#' @export
#'
visualize <- function(object, ...) {
    UseMethod("visualize")
}

#' @rdname visualize
#' @export
visualise <- function(object, ...) {
    UseMethod("visualize")
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


if(getRversion() >= "2.15.1")  {
    utils::globalVariables(c("author", "package", "version", "from", "n_depended_by", "n_depends", "n_enhanced_by", "n_enhances", "n_imported_by", "n_imports", "n_suggested_by", "n_suggests", "to", "type", "n_collaborators", "maintainer"))
}


