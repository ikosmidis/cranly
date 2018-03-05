#' @export
prepare_CRAN_network <- function(object, trace = FALSE) {

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
    edges_CRAN <- rbind(im, su, en, de)

    ## n_im_by <- stack(Matrix::rowSums(im[]))
    n_im_by <- stack(table(im$from))
    names(n_im_by) <- c("n_imported_by", "Package")
    ## n_im <- stack(Matrix::colSums(im[]))
    n_im <- stack(table(im$to))
    names(n_im) <- c("n_imports", "Package")
    ## n_de_by <- stack(Matrix::rowSums(de[]))
    n_de_by <- stack(table(de$from))
    names(n_de_by) <- c("n_depended_by", "Package")
    ## n_de <- stack(Matrix::colSums(de[]))
    n_de <- stack(table(de$to))
    names(n_de) <- c("n_depends", "Package")
    ## n_su_by <- stack(Matrix::rowSums(su[]))
    n_su_by <- stack(table(su$from))
    names(n_su_by) <- c("n_suggested_by", "Package")
    ## n_su <- stack(Matrix::colSums(su[]))
    n_su <- stack(table(su$to))
    names(n_su) <- c("n_suggests", "Package")
    ## n_en <- stack(Matrix::rowSums(en[]))
    n_en <- stack(table(en$from))
    names(n_en) <- c("n_enhances", "Package")
    ## n_en_by <- stack(Matrix::colSums(en[]))
    n_en_by <- stack(table(en$to))
    names(n_en_by) <- c("n_enhanced_by", "Package")

    nodes_CRAN <- Reduce(function(x, y) merge(x, y, by = "Package", all = TRUE), list(object, n_im, n_im_by, n_su, n_su_by, n_de, n_de_by, n_en, n_en_by))

    ## Remove packages with no version (e.g. coming from other repos like Bioconductor)
    nodes_CRAN <- subset(nodes_CRAN, !is.na(Version))
    ## Replace NA with zeros
    nodes_CRAN[grep("n_", names(nodes_CRAN))][is.na(nodes_CRAN[grep("n_", names(nodes_CRAN))])] <- 0

    out <- list(edges = edges_CRAN, nodes = nodes_CRAN)
    class(out) <- c("cranly_db_network", class(out))
    attr(out, "timestamp") <- attr(object, "timestamp")
    out
}
