# Copyright (C) 2018- Ioannis Kosmidis

#' Top-n package or author barplots according to a range of network statistics
#'
#' @param x a [`summary_cranly_network`] object.
#' @param top integer. How may top packages or authors should be plotted? Default is `20`.
#' @param according_to the statistic according to which the top-`top` list is produced. See [`summary.cranly_network`] for available statistics.
#' @param scale logical. Should the statistics be scaled to lie between `0` and `1` before plotting? Default is [`FALSE`].
#' @param ... currently not used
#'
#' @examples
#'
#' \donttest{
#' data("package_network", package = "cranly")
#' package_summaries <- summary(package_network)
#' plot(package_summaries, according_to = "n_imported_by", top = 30)
#' plot(package_summaries, according_to = "n_depended_by", top = 30)
#' plot(package_summaries, according_to = "page_rank", top = 30)
#'
#' ## author network
#' data("author_network", package = "cranly")
#' author_summaries <- summary(author_network)
#' plot(author_summaries, according_to = "n_collaborators", top = 30)
#' plot(author_summaries, according_to = "n_packages", top = 30)
#' plot(author_summaries, according_to = "page_rank", top = 30)
#' }
#'
#' @export
plot.summary_cranly_network <- function(x, top = 20, according_to = NULL, scale = FALSE, ...) {
    if (is.null(x)) {
        message("The supplied object has no package or author information. Nothing to plot.")
        return(invisible(NULL))
    }
    perspective <- attr(x, "perspective")
    if (perspective == "package") {
        what <- "package"
        nam <- x$package
        x$package <- NULL
        if (is.null(according_to))
            according_to <- "n_imported_by"
    }
    else {
        what <- "author"
        nam <- x$author
        x$author <- NULL
        if (is.null(according_to))
            according_to <- "n_packages"
    }
    stats <- colnames(x)
    according_to <- match.arg(according_to, stats)
    x[[what]] <- nam
    if (scale) {
        r <- range(x[[according_to]], na.rm = TRUE)
        x[[according_to]] <- (x[[according_to]] - r[1])/diff(r)
    }
    v <- x[rev(head(order(x[[according_to]], decreasing = TRUE), top)), c(what, according_to)]
    v[[what]] <- factor(v[[what]], ordered = TRUE, levels = v[[what]])

    out <- ggplot2::ggplot(v) +
        ggplot2::geom_bar(ggplot2::aes_string(x = what, y = according_to), stat = "identity") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = paste0("cranly top-", top, " according to ", according_to),
                      subtitle = paste("Package database as of", format(attr(x, "timestamp"))),
                      y = if (scale) paste(according_to, "score") else according_to) +
        ggplot2::coord_flip()
    print(out)
    invisible(v)
}
