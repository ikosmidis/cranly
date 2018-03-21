#' Clean and organise package and author names in the output of \code{tools::CRAN_package_db()}
#'
#' @aliases cranly_db
#'
#' @param packages_db a \code{\link{data.frame}} with the same
#'     structure to the output of \code{\link[tools]{CRAN_package_db}}
#'     (default).
#' @param clean_directives a function that transforms the contents of
#'     the various directives in the package descriptions to vectors
#'     of package names. Default is \code{\link{clean_up_directives}}.
#' @param clean_author a function that transforms the contents of
#'     \code{Author} to vectors of package authors. Default is
#'     \code{\link{clean_up_author}}.
#'
#' @details
#'
#' \code{clean_CRAN_db} uses \code{clean_up_directives} and
#' \code{clean_up_authors} to clean up the author names and package
#' names in the various directives (like \code{Imports},
#' \code{Depends}, \code{Suggests}, \code{Enhances}, as in the
#' \code{data.frame} that results from
#' \code{\link[tools]{CRAN_package_db}}) and return an organised
#' \code{data.frame} of class \code{cranly_db} that can be used for
#' further analysis.
#'
#'
#' The function tries hard to identify and eliminate mistakes in the
#' Author field of the description file, and extract a clean list of
#' only author names. The relevant operations are coded in the
#' \code{\link{clean_up_author}} function. The current version of
#' \code{\link{clean_up_author}} is far from best practice in using
#' regex but it currently does a fair job in cleaning up messy Author
#' fields. It will be improving in future versions.
#'
#' Custom clean-up functions can also be supplied via the
#' \code{clean_directives} and \code{clean_author} arguments.
#'
#' @return
#'
#' An \code{\link{data.frame}} with the same variables as
#' \code{packaged_db} (but with lower case names), a \code{timestamp}
#' attribute and also inheriting from \code{class_db}.
#'
#' @examples
#' \dontrun{
#' ## Before cleaning
#' cran_db <- tools::CRAN_package_db()
#' cran_db[cran_db$Package == "weights", "Author"]
#'
#' ## After clean up
#' package_db <- clean_CRAN_db(cran_db)
#' package_db[package_db$Package == "weights", "Author"]
#' }
#' @export
clean_CRAN_db <- function(packages_db = tools::CRAN_package_db(),
                          clean_directives = clean_up_directives,
                          clean_author = clean_up_author) {

    ## Remove reducndant MD5 sum
    ind <- which(grepl("MD5sum", names(packages_db)))

    packages_db <- packages_db[-ind[1]]
    ## Remove duplicated pacakges
    packages_db <- packages_db[!duplicated(packages_db$MD5sum), ]

    ## Coerce variable names to lower case
    names(packages_db) <- tolower(names(packages_db))

    packages_db <- within(packages_db, {
        imports <- clean_directives(imports)
        depends <- clean_directives(depends)
        suggests <- clean_directives(suggests)
        enhances <- clean_directives(enhances)
        author <- clean_author(author)
    })

    attr(packages_db, "timestamp") <- Sys.time()

    class(packages_db) <- c("cranly_db", class(packages_db))
    packages_db
}

#' Clean up package directives
#'
#' @param variable a character string
#'
#' @return
#'
#' A list of one vector of character strings
#'
#' @examples
#' clean_up_directives("R (234)\n stats (>0.01),     base\n graphics")
#' @export
clean_up_directives <- function(variable) {
    variable %>%
        str_replace_all("\n", ",") %>%
        str_replace_all("\\([^()]*\\)", "") %>%
        str_replace_all(" ", "") %>%
        str_replace_all("\\bR,\\b", "") %>%
        str_replace_all("\\bR\\b", "") %>%
        str_split(",") %>%
        lapply(function(x) {
            out <- str_replace_all(x, ",|^\\s+|\\s+$", "")
            out[!(out == "")]
        })## eliminate remaining commas and leading and trailing whitespaces
}

#' Clean up author names
#'
#' @param variable a character string
#'
#' @return
#'
#' A list of one vector of character strings
#'
#' @examples
#' clean_up_author("The R Core team, Brian & with some assistance from Achim, Hadley; Kurt\n Portugal; Ireland; Italy; Greece; Spain")
#' @export
clean_up_author1 <- function(variable) {

    repl_symbols <- c("\\+", ";", " - ", "/", "&", "'s", "\t+", "\"|'",
                      "\\*", "\\)", ":", "\n", "\\[.*?\\]",
                      "\\(.*?\\)", "\\<.*?\\>", "^\\s+|\\s+$|\\s+(?=\\s)")
    ## "\\."

    repl_other_words <- c("a", "also", "and", "apart", "as", "based",
                          "by", "due", "during", "each", "for",
                          "from", "him", "in", "into", "its", "not",
                          "of", "off", "on", "or", "otherwise", "our",
                          "some", "that", "their", "these", "they",
                          "this", "to", "together", "under", "unless",
                          "up", "via", "we", "which", "whose", "with",
                          "the")

    repl_nouns <- c("advice", "algorithm", "author", "book", "case",
                    "code", "codebase", "collaboration", "comment",
                    "content", "contribution", "contributor",
                    "control", "copies", "copy", "copyright",
                    "correction", "creator", "data", "dataset",
                    "detail", "development", "direction",
                    "discrepancy", "distribution", "document",
                    "documentation", "domain", "earlier", "email",
                    "enhancement", "entropy", "estimation", "example",
                    "excerpt", "extension", "file", "font",
                    "fragment", "function", "guide", "help", "idea",
                    "implementation", "library", "maintainer",
                    "manual", "math", "method", "model",
                    "modification", "module", "moment", "note",
                    "notice", "other", "package", "part",
                    "participant", "pattern", "person", "professor",
                    "program", "provider", "project", "script",
                    "series", "set", "similarity", "software",
                    "source", "subroutine", "suggestion",
                    "supervision", "survey", "term", "testing",
                    "thanks", "time", "tool", "toolbox", "transform",
                    "transition", "turn", "utilities", "utility",
                    "version", "website", "work", "worldwide",
                    "wrapper", "number", "libraries", "packge",
                    "well") # add s

    repl_adjectives <- c("additional", "all", "assistance",
                         "available", "considerable", "creative",
                         "former", "general", "grateful", "interface",
                         "key", "low", "new", "numerous", "open",
                         "original", "original", "public", "recent",
                         "significant", "sole", "special", "standard",
                         "substantial", "support", "toplevel",
                         "unsafe", "written", "various")

    repl_verbs <- c("adapt", "adopt", "are", "assist", "author",
                    "base", "belong", "borrow", "change", "claim",
                    "collect", "compile", "contain", "contribute",
                    "create", "derive", "develop", "download",
                    "embed", "extract", "follow", "fork", "fund",
                    "give", "go", "have", "include", "incorporate",
                    "interface", "is", "learn", "limit", "maintain",
                    "modified", "modify", "note", "order", "plot",
                    "port", "prepare", "provide", "publish", "relate",
                    "release", "remove", "represent", "revise", "see",
                    "set", "snip", "transfer", "translate", "use",
                    "was", "were", "been", "copied", "did",
                    "embedded", "had", "has", "made", "list") # Add d ed s ing (remove e if last)

    repl_proglang <- c("C", "Fortran", "JAVA", "MATLAB", "S-Plus",
                       "L-", "Python", "S", "Perl")

    repl_software <- c("loess", "tsvq", "rastamat", "ttice", "qrng",
                       "GUDHI", "BiSSE-ness", "simpls", "simplsfit",
                       "Onigmo", "readdcf", "RSvgDevice",
                       "randomForest", "fbvpot", "fbvpot", "rastamat",
                       "libmad", "MPEG", "audio", "decoder",
                       "getHostnameSystem", "Rutils", "mdamars",
                       "seewave", "listserv", "ANN", "GNU", "General",
                       "Public", "License", "readxportR", "Hmisc",
                       "zlib", "Cards", "argparse",
                       "Python Software Foundation", "getopt",
                       "Plan 9", "dlib", "histsu", "qqglddefault",
                       "readMP3", "tuneR", "cstratapsa", "mtxexp",
                       "leaps", "gld", "libhunspell",
                       "Apache Commons Codec", "multicore",
                       "yale sparse matrix", "GeoSSE",
                       "suite Quadpack", "Royset", "XGobi",
                       "libhunspell", "lmgls", "lm", "ARPACK",
                       "SCEoptim", "MsgPack", "QuantLib", "BOOM",
                       "Cephes", "Cuba", "mnormt", "foreignh",
                       "foreign", "rPython", "seewave", "fprintf",
                       "ibm2ieeec", "BRL-CAD", "sound", "ieee2ibmc",
                       "CppJieba", "Eigen", "cddlib", "R2OpenBUGS",
                       "R2WinBUGS", "TTBOX", "Leaflet", "XGobi",
                       "tem", "bicubuc", "ttice", "lattice", "pm")


    repl_techinal <- c("CPU", "Windows", "Mac", "OS", "X", "README",
                       "Fourier", "IO", "http")

    repl_adverbs <- c("largely", "originally", "previously",
                      "randomly", "substantially")

    repl_other <- c("\\\\code\\{hwexact\\}", "\\@R")

    repl_replacements <- c("DePauw University",
                           "Minister of Natural Resources Canada",
                           "National Institute on Drug Abuse Award",
                           "Zhejiang university",
                           "school of medicine", "Sampson-Guttorp",
                           "R-port", "skeleton creation",
                           "un-safe pointer arithmetics", "r-cran",
                           "United States Government",
                           "US Army Research Laboratory",
                           "Regents of the University of California",
                           "Intramural Research Program", "22e5",
                           "Alcoholism Award Number R03 AA019775",
                           "Regularized random forest for regression",
                           "Iowa State University",
                           "Ferdowsi University Of Mashhad",
                           "University of Edinburgh")

    repl_lists <- "R-help"

    repl01 <- paste(repl_symbols, collapse = "|")
    repl02 <- paste0("(?i)\\b", repl_other_words, "\\b", collapse = "|")
    repl03 <- paste0("(?i)\\b", repl_nouns, "\\b", collapse = "|")
    repl04 <- paste0("(?i)\\b", repl_nouns, "s\\b", collapse = "|")
    repl05 <- paste0("(?i)\\b", repl_adjectives, "\\b", collapse = "|")
    repl06 <- paste0("(?i)\\b", repl_verbs, "\\b", collapse = "|")
    repl07 <- sapply(repl_verbs, function(x) {
        if (substr(x, nchar(x), nchar(x)) == "e") paste0(x, "d")
        else paste0(x, "ed")
    })
    repl07 <- paste0("(?i)\\b", repl07, "\\b", collapse = "|")
    repl08 <- paste0("(?i)\\b", repl_verbs, "s\\b", collapse = "|")
    repl08 <- paste0("(?i)\\b", repl08, "\\b", collapse = "|")
    repl09 <- sapply(repl_verbs, function(x) {
        if (substr(x, nchar(x), nchar(x)) == "e") paste0(substr(x, 0, nchar(x) - 1), "ing")
        else  paste0(x, "ing")
    })
    repl09 <- paste0("(?i)\\b", repl09, "\\b", collapse = "|")
    repl10 <- paste0("(?i)\\b", repl_proglang, "\\b", collapse = "|")
    repl11 <- paste0("(?i)\\b", repl_software, "\\b", collapse = "|")
    repl12 <- paste0("(?i)\\b", repl_techinal, "\\b", collapse = "|")
    repl13 <- paste0("(?i)\\b", repl_adverbs, "\\b", collapse = "|")
    repl14 <- paste0("(?i)\\b", repl_other, "\\b", collapse = "|")
    repl15 <- paste0("(?i)\\b", repl_lists, "\\b", collapse = "|")
    repl00 <- paste0("(?i)\\b", repl_replacements, "\\b", collapse = "|")

    variable %>%
        str_replace_all(", Inc", " Inc") %>%
        str_replace_all(paste(countrycode::codelist$country.name.en, collapse = "|"), "") %>% ## eliminate country names
        str_replace_all("\\.", "") %>%
        str_replace_all(paste0("(?i)\\b", letters, "\\b", collapse = "|"), "") %>%
        str_replace_all(repl00, "") %>%
        str_replace_all(repl01, "") %>%
        str_replace_all(repl02, ",") %>%
        str_replace_all(repl03, ",") %>%
        str_replace_all(repl04, ",") %>%
        str_replace_all(repl05, ",") %>%
        str_replace_all(repl06, ",") %>%
        str_replace_all(repl07, ",") %>%
        str_replace_all(repl08, ",") %>%
        str_replace_all(repl09, ",") %>%
        str_replace_all(repl10, ",") %>%
        str_replace_all(repl11, ",") %>%
        str_replace_all(repl12, ",") %>%
        str_replace_all(repl13, ",") %>%
        str_replace_all(repl14, ",") %>%
        str_replace_all(repl15, ",") %>%
        str_replace_all("\\d{4}", "") %>%
        str_replace_all("\\d{2}", "") %>%
        str_replace_all("R Core Deveopment Team|R[- ][Cc]ore [Tt]eam|R [Dd]evelopment [Tt]eam|R[- ][Cc]ore [Dd]evelopment [Tt]eam|R core|R[- ][Cc]ore|R[- ][Dd]evelopment [Cc]ore [Tt]eam", "R Core") %>%
        str_replace_all("[Cc][Rr][Aa][Nn] [Tt]eam", "CRAN Team") %>%
        str_replace_all("R[ sS]tudio|R[ sS]tudio Inc", "RStudio") %>%
        str_replace_all("Wickham Hadley|Hadley Wickham function", "Hadley Wickham") %>%
        str_replace_all("Yihui Xie function", "Yihui Xie") %>%
        ## iconv(from = "UTF8", to = "ASCII//TRANSLIT") %>%
        str_split(",") %>%
        lapply(function(x) {
            out <- str_replace_all(x, ",|^\\s+|\\s+$", "")
            out[!(out == "")]
        })
}

