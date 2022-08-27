# Copyright (C) 2019- Ioannis Kosmidis

#' wordcloud of author names, package descriptions, and package titles
#'
#' @aliases word_cloud
#' @inheritParams subset.cranly_network
#' @inheritParams compute_term_frequency
#' @inheritParams wordcloud::wordcloud
#' @param x either a  [`cranly_network`] object or a named vector of term frequencies (typically the output of [compute_term_frequency()] with `frequency = "term"`.
#' @param random_order should words be plotted in random order? If \code{FALSE} (default) words are plotted in decreasing frequency.
#' @param perspective should the wordcloud be that of package descriptions (`perspective = "description"`; default), of package titles (`perspective = "title"`) or of author names (`perspective = "author"`).
#' @param ... other arguments to be passed to [wordcloud::wordcloud] (except `random.order` which is already defined through `random_order`).
#'
#'
#' @details
#'
#' When applied to [`cranly_network`] objects, [word_cloud()] subsets
#' either according to `author` (using the intersection of the result
#' of [author_of()] and [author_with()]) or according to package
#' (using the intersection of the results of [package_with()] and
#' [package_by()]).
#'
#' For handling more complex queries, one can manually extract the #'
#' term frequencies from a supplied vector of character strings (see
#' [compute_term_frequency()]), and use [word_cloud()] on them. See the
#' examples.
#'
#' @return
#'
#' A word cloud.
#'
#' @seealso [compute_term_frequency()]
#'
#' @examples
#' \donttest{
#' ## Package directives network
#' cran_db <- clean_CRAN_db()
#' package_network <- build_network(cran_db)
#' ## Descriptions of all packages in tidyverse
#' tidyverse <- imported_by(package_network, "tidyverse", exact = TRUE)
#' set.seed(123)
#' word_cloud(package_network, package = tidyverse, exact = TRUE, min.freq = 2)
#'
#' ## or by manually creating the term frequencies from descriptions
#' descriptions <- descriptions_of(package_network, tidyverse, exact = TRUE)
#' term_freq <- compute_term_frequency(descriptions)
#' set.seed(123)
#' word_cloud(term_freq, min.freq = 2)
#' }
#'
#' @export
word_cloud.cranly_network <- function(x,
                                     package = Inf,
                                     author = Inf,
                                     maintainer = Inf,
                                     base = TRUE,
                                     recommended = TRUE,
                                     exact = TRUE,
                                     perspective = "description",
                                     random_order = FALSE,
                                     ignore_words = c("www.jstor.org", "www.arxiv.org",
                                                      "arxiv.org", "provides", "https"),
                                     stem = FALSE,
                                     colors = rev(colorspace::heat_hcl(10)),
                                     ...) {

    if (!has_usable_data(x)) {
        message("Nothing to plot")
        return(invisible(NULL))
    }

    perspective <- match.arg(perspective, c("title", "description", "author"))

    x <- subset(x, base = base, recommended = recommended)

    if (all(is.infinite(package)) & all(is.infinite(author)) & all(is.infinite(maintainer))) {
        packages <- Inf
        authors <- authors_of(x, Inf)
    }
    else {
        if (perspective == "author") {
            a1 <- author_with(x, name = author, exact = exact)
            a2 <- author_of(x, package = package, exact = exact)
            authors <- intersect(a1, a2)
            if (!is.infinite(maintainer)) {
                a3 <- maintainer_of(x, package = package, exact = exact)
                authors <- intersect(authors, a3)
            }
        }
        else {
            p1 <- package_with(x, name = package, exact = exact)
            p2 <- package_by(x, author = author, exact = exact)
            packages <- intersect(p1, p2)
            if (!is.infinite(maintainer)) {
                p3 <- maintained_by(x, author = maintainer, exact = exact)
                packages <- intersect(packages, p3)
            }
        }
    }

    txt <- switch(perspective,
                  "author" = authors,
                  "title" = title_of(x, package = packages, exact = TRUE),
                  "description" = description_of(x, package = packages, exact = TRUE))

    term_freq <- compute_term_frequency(txt, ignore_words = ignore_words, stem = stem)
    term_freq <- sort(term_freq)

    word_cloud.numeric(term_freq, random_order = random_order, colors = colors, ...)

    invisible(term_freq)
}

#' @rdname word_cloud.cranly_network
#' @export
word_cloud.numeric <- function(x, random_order = FALSE, colors = rev(colorspace::heat_hcl(10)), ...) {
    wordcloud(words = names(x), freq = x, random.order = random_order,
              colors = colors, ...)
}

#' Compute term frequencies from a vector of text
#'
#' @param txt a vector of character strings.
#' @param ignore_words a vector of words to be ignored when forming the corpus.
#' @param stem should words be stemmed using Porter's stemming algorithm? Default is \code{FALSE}. See [tm::stemDocument()].
#' @param remove_punctuation should punctuation be removed when forming the corpus? Default is \code{TRUE}. See [tm::removePunctuation()].
#' @param remove_stopwords should english stopwords be removed when forming the corpus? Default is \code{TRUE}. See [tm::removeWords] and [tm::stopwords].
#' @param remove_numbers should numbers be removed when forming the corpus? Default is \code{TRUE}. See [tm::removeNumbers].
#' @param to_lower should all terms be coerced to lower-case when forming the corpus? Default is \code{TRUE}.
#' @param frequency the type of term frequencies to return. Options are `"term"` (default; a named vector of term frequencies), `"document-term"` (a document-term frequency matrix; see [tm::TermDocumentMatrix()]), `"term-document"` (a term-document frequency matrix; see [tm::DocumentTermMatrix()]).
#'
#' The operations are taking place as follows: remove special
#' characters, covert to lower-case (depending on the values of
#' `to_lower`), remove numbers (depending on the value of
#' `remove_numbers`), remove stop words (depending on the value of
#' `remove_stopwords`), remove custom words (depending on the value of
#' `ignore_words`), remove punctuation (depending on the value of
#' `remove_punctuation`), clean up any leading or trailing whitespace,
#' and, finally stem words (depending on the value of `stem`).
#'
#'
#' @return
#' Either a named numeric vector (`frequency = "term"`), or an object of class [tm::DocumentTermMatrix] (`frequency = "document-term"`), or or an object of class [`tm::TermDocumentMatrix`] (`frequency = "term-document"`).
#'
#' @details
#'
#' If `txt` is a named vector then the names are used as document id's
#' when forming the corpus.
#'
#' @seealso [word_cloud()]
#'
#' @export
compute_term_frequency <- function(txt,
                                   ignore_words = c("www.jstor.org", "www.arxiv.org",
                                                    "arxiv.org", "provides", "https"),
                                   stem = FALSE,
                                   remove_punctuation = TRUE,
                                   remove_stopwords = TRUE,
                                   remove_numbers = TRUE,
                                   to_lower = TRUE,
                                   frequency = "term") {
    frequency <- match.arg(frequency, c("term", "document-term", "term-document"))
    doc_id <- if (is.null(names(txt))) seq_along(txt) else names(txt)
    txt <- data.frame(doc_id = doc_id, text = txt, stringsAsFactors = FALSE)
    corpa <- Corpus(DataframeSource(txt))
    to_space <- content_transformer(function (x, p) str_replace_all(x, p, " "))
    corpa <- tm_map(corpa, to_space, "/")
    corpa <- tm_map(corpa, to_space, "@")
    corpa <- tm_map(corpa, to_space, "\\|")
    if (isTRUE(to_lower)) {
        corpa <- tm_map(corpa, content_transformer(tolower))
    }
    if (isTRUE(remove_numbers)) {
        corpa <- tm_map(corpa, removeNumbers)
    }
    if (isTRUE(remove_stopwords)) {
        corpa <- tm_map(corpa, removeWords, stopwords("english"))
    }
    if (!is.null(ignore_words)) {
        corpa <- tm_map(corpa, removeWords, ignore_words)
    }
    if (isTRUE(remove_punctuation)) {
        corpa <- tm_map(corpa, removePunctuation,
                        preserve_intra_word_contractions = TRUE,
                        preserve_intra_word_dashes = TRUE)
    }
    corpa <- tm_map(corpa, stripWhitespace)
    if (stem) {
        corpa <- tm_map(corpa, stemDocument)
    }
    if (frequency == "document-term") {
        out <- DocumentTermMatrix(corpa)
    }
    else {
        term_document <- TermDocumentMatrix(corpa)
        if (frequency == "term-document") {
            out <- term_document
        }
        if (frequency == "term") {
            out <- rowSums(as.matrix(term_document))
        }
    }
    out
}
