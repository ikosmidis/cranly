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
clean_up_directives1 <- function(variable) {
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

    repl_symbols0 <- c(";", "/", "&")
    ## "\\+"
    repl_symbols1 <- c(" - ", "'s", "\t+", "\"|'",
                       "\\*", "\\(", "\\)", ":")
    ## "\\." "\n"

    repl_other_words0 <- c("and", "with", "by", "due", "from")
    repl_other_words1 <- c("a", "also", "apart", "as", "based",
                           "during", "each", "for", "him", "in",
                           "into", "its", "not", "of", "off", "on",
                           "or", "otherwise", "our", "some", "that",
                           "their", "these", "they", "this", "to",
                           "together", "under", "unless", "up", "via",
                           "we", "which", "whose", "the", "both")


    repl_nouns0 <- c("advice", "algorithm", "author", "book", "case", "name",
                    "code", "codebase", "collaboration", "comment",
                    "content", "contribution", "contributor",
                    "control", "copies", "copy", "copyright",
                    "correction", "creator", "data", "dataset",
                    "detail", "development", "direction",
                    "discrepancy", "distribution", "document",
                    "documentation", "domain", "earlier", "email",
                    "enhancement", "entropy", "estimation", "example",
                    "excerpt", "extension", "file", "font",
                    "fragment", "function", "guide", "group",
                    "help", "idea",
                    "implementation", "library", "maintainer",
                    "manual", "math", "method", "model",
                    "modification", "module", "moment", "note",
                    "notice", "other", "package", "part",
                    "participant", "pattern", "person", "professor",
                    "program", "provider", "project", "script",
                    "series", "set", "similarity", "software",
                    "source", "subroutine", "routine", "suggestion",
                    "supervision", "survey", "term", "testing",
                    "thanks", "time", "tool", "toolbox", "transform",
                    "transition", "turn", "utilities", "utility",
                    "version", "website", "work", "worldwide",
                    "wrapper", "number", "libraries", "packge",
                    "minister", "queen", "majesty", "right",
                    "resource") # add s
    repl_nouns1 <- paste0(repl_nouns0, "s")

    repl_adjectives0 <- c("additional", "all", "assistance", "initial",
                         "available", "considerable", "creative",
                         "former", "general", "grateful", "interface",
                         "key", "low", "new", "numerous", "open",
                         "original", "original", "public", "recent",
                         "significant", "sole", "special", "standard",
                         "substantial", "support", "toplevel",
                         "unsafe", "written", "various", "natural")

    repl_verbs0 <- c("adapt", "adopt", "are", "assist", "author",
                    "base", "belong", "borrow", "change", "claim",
                    "collect", "compile", "contain", "contribute",
                    "cover",
                    "create", "derive", "develop", "download",
                    "embed", "extract", "follow", "fork", "fund",
                    "give", "go", "have", "include", "incorporate",
                    "interface", "is", "learn", "limit", "maintain",
                    "modified", "modify", "note", "order", "plot",
                    "port", "prepare", "provide", "publish", "relate",
                    "release", "remove", "represent", "revise", "see",
                    "set", "snip", "snipped", "transfer", "translate", "use",
                    "was", "were", "been", "copied", "did",
                    "embedded", "had", "has", "made", "list") # Add d ed s ing (remove e if last)
    repl_verbs1 <- sapply(repl_verbs0, function(x) {
        if (substr(x, nchar(x), nchar(x)) == "e") paste0(x, "d")
        else paste0(x, "ed")
    })
    repl_verbs2 <- paste0(repl_verbs0, "s")
    repl_verbs3 <- sapply(repl_verbs0, function(x) {
        if (substr(x, nchar(x), nchar(x)) == "e") paste0(substr(x, 0, nchar(x) - 1), "ing")
        else  paste0(x, "ing")
    })

    repl_proglang0 <- c("Fortran-77", "Fortran", "JAVA", "MATLAB", "S-Plus",
                       "L-", "Python", "Perl")#, "R", "S", "C")

    repl_software0 <- c("ars", "loess", "tsvq", "rastamat", "ttice",
                        "ppexp", "es5-shim",
                        "qrng", "GUDHI", "BiSSE-ness", "simpls",
                        "simplsfit", "Onigmo", "readdcf",
                        "RSvgDevice", "randomForest", "fbvpot",
                        "fbvpot", "rastamat", "libmad", "MPEG",
                        "audio", "decoder", "getHostnameSystem",
                        "Rutils", "mdamars", "seewave", "listserv",
                        "GNU", "General", "Public", "License",
                        ## "ANN",
                        "readxportR", "Hmisc", "zlib", "Cards",
                        "argparse", "Python Software Foundation",
                        "getopt", "Plan 9", "dlib", "histsu",
                        "qqglddefault", "readMP3", "tuneR",
                        "cstratapsa", "mtxexp", "leaps", "gld",
                        "libhunspell", "Apache Commons Codec",
                        "multicore", "yale sparse matrix", "GeoSSE",
                        "suite Quadpack", "Royset", "XGobi",
                        "libhunspell", "lmgls", "lm", "ARPACK",
                        "SCEoptim", "MsgPack", "QuantLib", "BOOM",
                        "Cephes", "Cuba", "mnormt", "foreignh",
                        "foreign", "rPython", "seewave", "fprintf",
                        "ibm2ieeec", "BRL-CAD", "sound", "ieee2ibmc",
                        "CppJieba", "Eigen", "cddlib", "R2OpenBUGS",
                        "R2WinBUGS", "TTBOX", "Leaflet", "XGobi",
                        "tem", "bicubuc", "bicubic", "ttice",
                        "lattice", "pm", "ALINE", "NEWUOA")

    repl_technical0 <- c("CPU", "Windows", "Mac", "README",
                         "Fourier", "IO", "http", "Linux")

    repl_adverbs0 <- c("largely", "originally", "previously",
                       "randomly", "substantially", "equally",
                       "well")

    repl_replacements0 <- c("based on", "based on earlier work by")
    repl_replacements1 <- c("DePauw University",
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
                           "University of Edinburgh",
                           "Australian Bureau of Statistics",
                           "TOMS",
                           "Trustees of Columbia University",
                           "copyright holder",
                           "Ragnar Frisch Centre for Economic Research",
                           "Oslo",
                           "Institute for Defense Analyses",
                           "Finnish Institute of Occupational Health",
                           "National Agrarian University La Molina",
                           "Agriculture Ministry of Peru",
                           "Agronomy Faculty")

    repl000 <- c("simpls\\.fit", "\\.f", "S\\s+original", "\\\\code\\{hwexact\\}",
                 "R\\s+port", "R\\s+code", "Decision Patterns",
                 "Free Software Foundation", "mtx\\.exp",
                 "C-code", "jQuery contributors", "MathQuill contributors",
                 "Ph\\.D\\.")

    repl_lists0 <- "R-help"

    ## firstup <- function(x) stri_trans_totitle(x, opts_brkiter = stri_opts_brkiter(type = "sentence"))

    split_and_replace <- function(string, base_patterns, replacement,
                                  nparts = ceiling(length(base_patterns)/5), fixed_word = TRUE) {
        ns <- length(base_patterns)
        np <- floor(ns/nparts)
        u <- c(rep(np, nparts - 1), ns - np * (nparts - 1))
        parts <- rep(seq.int(nparts), u)
        if (fixed_word) {
            out <- lapply(split(base_patterns, parts), function(x) {
                paste0("(?i)\\b", x, "\\b", collapse = "|")
            })
        }
        else {
            out <- lapply(split(base_patterns, parts), function(x) {
                paste0("(?i)", x, collapse = "|")
            })
        }
        for (j in seq_along(out)) {
            string <- str_replace_all(string, out[[j]], replacement)
        }
        string
    }

    ## Remove hopeless entries
    inds <- grepl("Authors\\@R|c\\(", variable)
    variable[inds] <- ""

    variable %>%
        split_and_replace(repl000, "", fixed_word = FALSE) %>%
        str_replace_all(", Inc", " Inc") %>%
        str_replace_all("\\[(.|\n)*?\\]", "") %>% ## remove all [aut, ctb etc]
        str_replace_all("\\((.|\n)*?\\)", "") %>% ## remove anything in parenthesis; assuming that author names are NOT in parentheses
        str_replace_all("\\<(.|\n)*?\\>", "") %>% ## remove anything in <>
        str_replace_all("\\.", " ") %>%
        str_replace_all(paste(repl_symbols0, collapse = "|"), ",") %>%
        str_replace_all(paste(repl_symbols1, collapse = "|"), "") %>%
        ## str_replace_all(paste0("(?i)\\b", letters, "\\b", collapse = "|"), "") %>%
        str_replace_all("^\\s+|\\s+$|\\s+(?=\\s)", "") %>% ## eliminate white space
        split_and_replace(repl_replacements0, ",") %>%
        split_and_replace(repl_replacements1, "") %>%
        split_and_replace(repl_software0, "") %>%
        split_and_replace(repl_proglang0, "") %>%
        str_replace_all("C\\+\\+", "") %>%
        str_replace_all("\\+", ",") %>%
        split_and_replace(repl_other_words0, ",") %>%
        split_and_replace(repl_other_words1, "") %>%
        split_and_replace(repl_nouns0, "") %>%
        split_and_replace(repl_nouns1, "") %>%
        split_and_replace(repl_adjectives0, "") %>%
        split_and_replace(repl_verbs0, "") %>%
        split_and_replace(repl_verbs1, "") %>%
        split_and_replace(repl_verbs2, "") %>%
        split_and_replace(repl_verbs3, "") %>%
        split_and_replace(repl_technical0, "") %>%
        split_and_replace(repl_adverbs0, "") %>%
        split_and_replace(repl_lists0, "") %>%
        str_replace_all(paste(countrycode::codelist$country.name.en, collapse = "|"), "") %>% ## eliminate country names
        str_replace_all("\\d{4}", "") %>%
        str_replace_all("\\d{3}", "") %>%
        str_replace_all("\\d{2}", "") %>%
        str_replace_all("R\\s+Core\\s+Deveopment\\s+Team|R(-|\\s+)[Cc]ore\\s+[Tt]eam|R\\s+[Dd]evelopment\\s+[Tt]eam|R(-|\\s+)[Cc]ore\\s+[Dd]evelopment\\s+[Tt]eam|R\\s+core|R(-|\\s+)[Cc]ore|R(-|\\s+)[Dd]evelopment\\s+[Cc]ore\\s+[Tt]eam", "R Core") %>%
        str_replace_all("[Cc][Rr][Aa][Nn] [Tt]eam", "CRAN Team") %>%
        str_replace_all("R[ sS]tudio|R[ sS]tudio Inc", "RStudio") %>%
        str_replace_all("Wickham Hadley|Hadley Wickham function", "Hadley Wickham") %>%
        str_replace_all("Yihui Xie function", "Yihui Xie") %>%
        ## iconv(from = "UTF8", to = "ASCII//TRANSLIT") %>%
        str_split(",") %>%
        lapply(function(x) {
            x <- str_replace_all(x, ",|^\\s+|\\s+$|\\s+(?=\\s)", "")
            unique(x[!(x == "" | x == " " | x %in% letters | x %in% LETTERS)])
        })

}


## If it starts with A do not remove A
## If it ends with R, S, A remove R
## If there is a programmer named "R", "S", "A" remove
## Remove single letters
