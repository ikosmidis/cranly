#' cranly: CRAN package database analytics and visualizations
#'
#' @docType package
#' @name cranly
#' @import igraph
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_split
#' @importFrom utils combn stack
#' @import countrycode
#' @importFrom ggplot2 ggplot geom_bar theme_minimal labs coord_flip
#'
NULL

#' @export
author_of <- function(object, ...) {
    UseMethod("author_of")
}

#' @export
package_by <- function(object, ...) {
    UseMethod("package_by")
}

#' @export
visualize <- function(object, ...) {
    UseMethod("visualize")
}

#' @export
build_network <- function(object, ...) {
    UseMethod("build_network")
}


if(getRversion() >= "2.15.1")  {
    utils::globalVariables(c("Author", "Package", "Version", "from", "n_depended_by", "n_depends", "n_enhanced_by", "n_enhances", "n_imported_by", "n_imports", "n_suggested_by", "n_suggests", "to", "type"))
}


