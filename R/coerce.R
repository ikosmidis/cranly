as.igraph.cranly_network <- function(object, reverse = TRUE, ...) {

    perspective <- attr(object, "perspective")

    edges <- object$edges
    nodes <- object$nodes

    if (perspective == "package") {
        v_names <- c("Package", "Version", "Author", "Date", "URL",
                     "n_imports", "n_imported_by",
                     "n_suggests", "n_suggested_by",
                     "n_depends", "n_depended_by",
                     "n_enhances", "n_enhanced_by")
        e_inds <- if (reverse) c(2, 1, 3:ncol(edges)) else 1:ncol(edges)
        g <- graph.data.frame(edges[, e_inds], vertices = nodes[v_names], directed = TRUE)
        E(g)$type <-  edges$type
    }
    else {
	vnames <- c("Author", "Package")
        g <- graph.data.frame(edges, vertices = nodes[vnames], directed = FALSE)
        E(g)$package <- edges$Package
        E(g)$imports <- edges$Imports
        E(g)$depends <- edges$Depends
        E(g)$suggests <- edges$Suggests
        E(g)$enhances <- edges$Enhances
        E(g)$version <- edges$Version
    }
    g

}

## see http://kateto.net/networks-r-igraph for intro
