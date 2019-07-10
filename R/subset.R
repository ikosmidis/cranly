# Copyright (C) 2018- Ioannis Kosmidis

#' Subset a [`cranly_network`] according to author, package and/or directive
#'
#' @param x a  [`cranly_network`] object.
#' @param package a vector of character strings with the package names to be matched. Default is [`Inf`] which returns all available packages in `x` for further subsetting.
#' @param author a vector of character strings with the author names to be matched. Default is `Inf` which returns all available author in `x` for further subsetting.
#' @param directive a vector of at least one of `"Imports"`, `"Suggests"`, `"Enhances"`, `"Depends"`.
#' @param base logical. Should we include base packages in the subset? Default is `TRUE`.
#' @param recommended  logical. Should we include recommended packages in the subset? Default is `TRUE`.
#' @param exact logical. Should we use exact matching? Default is `TRUE`.
#' @param only logical. If `TRUE` the subset includes only the edges between packages named in `package` and/or authors named in `author`. If `FALSE` (default) edges to and from all other packages and/or authors that are linked to `package` and/or `author` are included in the subset.
#' @param ... currently not used.
#'
#' @return
#' A [`cranly_network`] object that is the subject of `x`.
#' 
#' @export
subset.cranly_network <- function(x,
                                  package = Inf,
                                  author = Inf,
                                  directive = c("imports", "suggests", "enhances", "depends", "linkingto"),
                                  base = TRUE,
                                  recommended = TRUE,
                                  exact = TRUE,
                                  only = FALSE,
                                  ...) {
    perspective <- attr(x, "perspective")
    if (perspective == "package") {

        p1 <- package_with(x, name = package, exact = exact)
        p2 <- package_by(x, author = author, exact = exact)

        base_packages <- subset(x$nodes, priority == "base")$package
        recommended_packages <- subset(x$nodes, priority == "recommended")$package

        ## keep <- unique(c(p1, p2))
        keep <- intersect(p1, p2)
        
        if (only) {
            inds <- with(x$edges, (to %in% keep & from %in% keep) &
                                  (type %in% directive))
        }
        else {
            inds <- with(x$edges, (to %in% keep | from %in% keep) & (type %in% directive))
        }
        if (!base) {
            inds <- inds & with(x$edges, !(to %in% base_packages | from %in% base_packages))
        }
        if (!recommended) {
            inds <- inds & with(x$edges, !(to %in% recommended_packages | from %in% recommended_packages))
        }
        edges_subset <- subset(x$edges, inds)
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), keep))
        nodes_subset <- subset(x$nodes, package %in% node_names)
    }
    else {        
        a1 <- author_with(x, name = author, exact = exact)
        a2 <- author_of(x, package = package, exact = exact)
        
        ## keep <- unique(c(a1, a2))
        keep <- intersect(a1, a2)

        if (only) {
            edges_subset <- subset(x$edges, (to %in% keep & from %in% keep))
        }
        else {
            edges_subset <- subset(x$edges, (to %in% keep | from %in% keep))
        }
        node_names <- unique(c(as.character(edges_subset$from), as.character(edges_subset$to), keep))
        nodes_subset <- subset(x$nodes, author %in% node_names)
    }
    out <- list(edges = edges_subset, nodes = nodes_subset)
    attr(out, "timestamp") <- attr(x, "timestamp")
    attr(out, "perspective") <- perspective

    attr(out, "keep") <- keep
    class(out) <- c("cranly_network", class(out))
    out

}
