# Copyright (C) 2018 Ioannis Kosmidis

## firstup <- function(x) {
##     stri_trans_totitle(x, opts_brkiter = stri_opts_brkiter(type = "sentence"))
## }

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
