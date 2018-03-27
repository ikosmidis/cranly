# Copyright (C) 2018 Ioannis Kosmidis

#' Compute edges and nodes of package directives and collaboration networks
#'
#' @param object a \code{\link{cranly_db}} object
#' @param trace logical. Print progress information? Default is \code{FALSE}
#' @param perspective character. Build a \code{"package"} (default )or an \code{"author"} network?
#' @param ... Currently not used
#'
#' @aliases cranly_network
#'
#' @return
#'
#' A list of 2 \code{\link{data.frame}s} with the \code{edges} and
#' \code{nodes} of the network
#'
#' @details
#'
#' The convention for a \code{\link{cranly_network}} object with
#' \code{perspective = "package"} is that the direction of an edge is
#' from the package that is imported by, suggested by, enhances or is
#' a dependency of another package, to the latter package. The author
#' network is analysed and visualised as undirected by all methods in
#' \code{cranly}.
#'
#' @examples
#' \dontrun{
#' data("cran20032018", package = "cranly")
#' ## Package directives network
#' package_network <- build_network(object = cran20032018, perspective = "package")
#' head(package_network$edges)
#' head(package_network$nodes)
#' attr(package_network, "timestamp")
#' class(package_network)
#'
#' ## Author collaboration network
#' author_network <- build_network(object = cran20032018, perspective = "author")
#' head(author_network$edges)
#' head(author_network$nodes)
#' attr(author_network, "timestamp")
#' class(author_network)
#' }
#'
#' @export
build_network.cranly_db <- function(object = clean_CRAN_db(),
                                    trace = FALSE, perspective = "package", ...) {

    perspective <- match.arg(perspective, c("package", "author"))

    if (perspective == "package") {
        compute_edges <- function(what = "imports", rev = FALSE) {
            out <- object[[what]]
            names(out) <- object[["package"]]
            out <- stack(out[sapply(out, function(x) !all(is.na(x)))])
            out$ind <- as.character(out$ind)
            ## out <- stack(lapply(out, function(x) x[x != "" & x != "R"]))
            names(out) <- if (rev) c("to", "from") else c("from", "to")
            data.frame(out[, c("from", "to")], type = what, stringsAsFactors = FALSE)
        }

        im <- compute_edges(what = "imports")
        su <- compute_edges(what = "suggests")
        en <- compute_edges(what = "enhances", rev = TRUE)
        de <- compute_edges(what = "depends")

        ## Edges
        edges <- rbind(im, su, en, de)

        ## n_im_by <- stack(table(im$from))
        ## names(n_im_by) <- c("n_imported_by", "package")
        ## n_im <- stack(table(im$to))
        ## names(n_im) <- c("n_imports", "package")
        ## n_de_by <- stack(table(de$from))
        ## names(n_de_by) <- c("n_depended_by", "package")
        ## n_de <- stack(table(de$to))
        ## names(n_de) <- c("n_depends", "package")
        ## n_su_by <- stack(table(su$from))
        ## names(n_su_by) <- c("n_suggested_by", "package")
        ## n_su <- stack(table(su$to))
        ## names(n_su) <- c("n_suggests", "package")
        ## n_en <- stack(table(en$from))
        ## names(n_en) <- c("n_enhances", "package")
        ## n_en_by <- stack(table(en$to))
        ## names(n_en_by) <- c("n_enhanced_by", "package")
        ## nodes <- Reduce(function(x, y) merge(x, y, by = "package", all = TRUE), list(object, n_im, n_im_by, n_su, n_su_by, n_de, n_de_by, n_en, n_en_by))
        ## ## Replace NA with zeros
        ## nodes[grep("n_", names(nodes))][is.na(nodes[grep("n_", names(nodes))])] <- 0

        nodes <- merge(data.frame(package = unique(c(edges$from, edges$to)), stringsAsFactors=FALSE),
                       object, by = "package", all.x = TRUE)

    }
    else {
        edges <- apply(object, 1, function(x) {
            auth <- x$author
            if (length(auth) < 2)
                NULL
            else {
                d <- data.frame(t(combn(auth, 2)), stringsAsFactors = FALSE)
                d[["package"]] <- x$package
                d[["imports"]] <- unname(x["imports"])
                d[["suggests"]] <- unname(x["suggests"])
                d[["enhances"]] <- unname(x["enhances"])
                d[["depends"]] <- unname(x["depends"])
                d[["version"]] <- x$version
                d[["maintainer"]] <- x$maintainer
                d
            }
        })
        edges <- do.call("rbind", edges)

        names(edges)[1:2] <- c("from", "to")

        ## n_col <- with(edges, {table(c(from, to))})

        nodes <- do.call("rbind", apply(object[c("author", "package")], 1, function(x) {
                                   auth <- x$author
                                   matrix(c(auth, rep(x$package, length(auth))), ncol = 2)
                                  }))

        nodes <- do.call("rbind", lapply(unique(nodes[, 1]), function(auth) {
            d <- data.frame(author = auth, stringsAsFactors = FALSE)
            d[["package"]] <- list(nodes[nodes[, 1] == auth, 2])
            d
            }))

        ## nodes$n_collaborators <- as.vector(n_col[nodes$author])

    }

    out <- list(edges = edges, nodes = nodes)
    class(out) <- c("cranly_network", class(out))
    attr(out, "timestamp") <- attr(object, "timestamp")
    attr(out, "perspective") <- perspective
    out

}
