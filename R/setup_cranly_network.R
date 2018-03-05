#' Extract edges and nodes from a \code{\link{cranly_db}} object
#'
#' @examples
#' \dontrun{
#' data("cran_db_example", package = "cranly")
#' package_db <- clean_CRAN_db(cran_db_example)
#' package_network <- setup_cranly_network(packages_db)
#' head(package_network$edges)
#' head(package_network$nodes)
#' attr(package_network, "timestamp")
#' class(package_network)
#' }
#'
#'  @export
setup_cranly_network <- function(object = clean_CRAN_db(),
                                  trace = FALSE, perspective = "package") {

    perspective <- match.arg(perspective, c("package", "author"))


    if (perspective == "package") {
        compute_edges <- function(what = "Imports", rev = FALSE) {
            out <- object[[what]]
            names(out) <- object$Package
            out <- out[sapply(out, function(x) !all(is.na(x)))]
            out <- stack(lapply(out, function(x) x[x != "" & x != "R"]))
            names(out) <- if (rev) c("to", "from") else c("from", "to")
            data.frame(out[c("from", "to")], type = what, stringsAsFactors = FALSE)
        }

        im <- compute_edges(what = "Imports")
        su <- compute_edges(what = "Suggests")
        en <- compute_edges(what = "Enhances", rev = TRUE)
        de <- compute_edges(what = "Depends")

        ## Edges
        edges <- rbind(im, su, en, de)

        n_im_by <- stack(table(im$from))
        names(n_im_by) <- c("n_imported_by", "Package")
        n_im <- stack(table(im$to))
        names(n_im) <- c("n_imports", "Package")
        n_de_by <- stack(table(de$from))
        names(n_de_by) <- c("n_depended_by", "Package")
        n_de <- stack(table(de$to))
        names(n_de) <- c("n_depends", "Package")
        n_su_by <- stack(table(su$from))
        names(n_su_by) <- c("n_suggested_by", "Package")
        n_su <- stack(table(su$to))
        names(n_su) <- c("n_suggests", "Package")
        n_en <- stack(table(en$from))
        names(n_en) <- c("n_enhances", "Package")
        n_en_by <- stack(table(en$to))
        names(n_en_by) <- c("n_enhanced_by", "Package")

        nodes <- Reduce(function(x, y) merge(x, y, by = "Package", all = TRUE), list(object, n_im, n_im_by, n_su, n_su_by, n_de, n_de_by, n_en, n_en_by))

        ## Remove packages with no version (e.g. coming from other repos like Bioconductor)
        nodes <- subset(nodes, !is.na(Version))
        ## Replace NA with zeros
        nodes[grep("n_", names(nodes))][is.na(nodes[grep("n_", names(nodes))])] <- 0

    }
    else {

        edges <- apply(object, 1, function(x) {
            auth <- x$Author
            if (length(auth) < 2)
                NULL
            else {
                d <- data.frame(t(combn(auth, 2)))
                d[["Package"]] <- x$Package
                d[["Imports"]] <- unname(x["Imports"])
                d[["Suggests"]] <- unname(x["Suggests"])
                d[["Enhances"]] <- unname(x["Enhances"])
                d[["Depends"]] <- unname(x["Depends"])
                d[["Version"]] <- x$Version
                d[["Maintainer"]] <- x$Maintainer
                d
            }
        })

        ## Here

    }



    out <- list(edges = edges, nodes = nodes)
    class(out) <- c("cranly_network", class(out))
    attr(out, "timestamp") <- attr(object, "timestamp")
    attr(out, "perspective") <- attr(object, perspective)
    out

}
