#' @export
prepare_CRAN_network <- function(object, trace = FALSE) {

    compute_edges <- function(what = "Imports", rev = FALSE) {
        out <- object[[what]]
        names(out) <- object$Package
        out <- out[sapply(out, function(x) !all(is.na(x)))]
        out <- lapply(out, function(x) x[x != "" & x != "R"])
        out <- stack(out)
        graph.edgelist(as.matrix(if (rev) out[c(2, 1)] else out))
    }

    im <- compute_edges(what = "Imports")
    su <- compute_edges(what = "Suggests")
    en <- compute_edges(what = "Enhances", rev = TRUE)
    de <- compute_edges(what = "Depends")

    n_im_by <- Matrix::rowSums(im[])
    n_im <- Matrix::colSums(im[])
    n_de_by <- Matrix::rowSums(de[])
    n_de <- Matrix::colSums(de[])
    n_su_by <- Matrix::rowSums(su[])
    n_su <- Matrix::colSums(su[])
    n_en <- Matrix::rowSums(en[])
    n_en_by <- Matrix::colSums(en[])

    ## Old code below

    op <- options()
    options(dplyr.show_progress = trace)


    edges_CRAN <- do.call("rbind", apply(object, 1, function(x) {
        rbind(data.frame(from = unlist(x$Imports), to = x$Package, type = "import", stringsAsFactors = FALSE),
             data.frame(from = unlist(x$Suggests), to = x$Package, type = "suggest", stringsAsFactors = FALSE),
             data.frame(from = x$Package, to = unlist(x$Enhances), type = "enhance", stringsAsFactors = FALSE),
             data.frame(from = unlist(x$Depends), to = x$Package, type = "depend", stringsAsFactors = FALSE))
    }))

    ## Compute how many times package to depends, suggests, imports, enahnces
    n <- edges_CRAN %>%
        group_by(to, type) %>%
        summarize(n = ifelse(all(is.na(from)), as.integer(0), length(unique(from)))) %>%
        rename(id = to)

    ## Compute how many times from appears in depends, suggests, imports, enahnces
    n_by <- edges_CRAN %>%
        group_by(from, type) %>%
        summarize(n_by = ifelse(all(is.na(to)), as.integer(0), length(unique(to)))) %>%
        rename(id = from)

    nodes_CRAN <- tibble(id = object$Package) %>%
        left_join(n_by %>% filter(type == "import"), by = "id") %>%
        rename(n_imported_by = n_by) %>% select(-type) %>%
        left_join(n_by %>% filter(type == "suggest"), by = "id") %>%
        rename(n_suggested_by = n_by) %>% select(-type) %>%
        left_join(n_by %>% filter(type == "enhance"), by = "id") %>%
        rename(n_enhances = n_by) %>% select(-type) %>% ## n_by has enhances instead of enhanced_by
        left_join(n_by %>% filter(type == "depend"), by = "id") %>%
        rename(n_depended_by = n_by) %>% select(-type) %>%
        left_join(n %>% filter(type == "depend"), by = "id") %>%
        rename(n_depends = n) %>% select(-type) %>%
        left_join(n %>% filter(type == "enahnce"), by = "id") %>%
        rename(n_enhanced_by = n) %>% select(-type) %>% ## n has enhanced_by instead of enhances
        left_join(n %>% filter(type == "suggest"), by = "id") %>%
        rename(n_suggests = n) %>% select(-type) %>%
        left_join(n %>% filter(type == "import"), by = "id") %>%
        rename(n_imports = n) %>% select(-type) %>%
        replace_na(list(n_imported_by = as.integer(0),
                        n_suggested_by = as.integer(0),
                        n_depended_by = as.integer(0),
                        n_enhanced_by = as.integer(0),
                        n_imports = as.integer(0),
                        n_suggests = as.integer(0),
                        n_depends = as.integer(0),
                        n_enhances = as.integer(0)))

    ## Add versio and other info to nodes_CRAN_subset. Drop %>% and dplyr here
    res_ids <- match(nodes_CRAN$id, object$Package, nomatch = 0)
    nodes_CRAN[["version"]] <- object[res_ids, ]$Version
    nodes_CRAN[["author"]] <- object[res_ids, ]$Author
    nodes_CRAN[["licence"]] <- object[res_ids, ]$License

    options(op)

    out <- list(edges = edges_CRAN, nodes = nodes_CRAN)
    class(out) <- c("cranly_db_network", class(out))
    attr(out, "timestamp") <- attr(object, "timestamp")
    out
}
