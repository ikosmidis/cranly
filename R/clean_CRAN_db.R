#' Clean and organise directives and authors in the output of \code{tools::CRAN_package_db()}
#'
#' @aliases cranly_db
#'
#' @examples
#' \dontrun{
#' data("cran_db_example", package = "cranly")
#' package_db <- clean_CRAN_db(cran_db_example)
#' }
#' @export
clean_CRAN_db <- function(packages_db = tools::CRAN_package_db(),
                          clean_directives = clean_up_directives,
                          clean_author = clean_up_author) {

    ## Remove reducndant MD5 sum
    packages_db <- packages_db[-which(grepl("MD5sum", names(packages_db)))[1]]

    ## Remove duplicated pacakges
    packages_db <- packages_db[!duplicated(packages_db$MD5sum), ]

    packages_db <- within(packages_db, {
        Imports <- clean_directives(Imports)
        Depends <- clean_directives(Depends)
        Suggests <- clean_directives(Suggests)
        Enhances <- clean_directives(Enhances)
        Author <- clean_author(Author)
    })

    attr(packages_db, "timestamp") <- Sys.time()

    class(packages_db) <- c("cranly_db", class(packages_db))
    packages_db
}


#' @export
clean_up_directives <- function(variable) {
    variable %>%
        str_replace_all("\n", ",") %>%
        str_replace_all("\\([^()]*\\)", "") %>%
        str_replace_all(" ", "") %>%
        str_split(",")
}


#' @export
clean_up_author <- function(variable) {
    variable %>%
        str_replace_all("\n", "") %>%
        str_replace_all(", Inc", " Inc") %>%
        str_replace_all("\\[.*?\\]", "") %>% ## remove all [aut, ctb etc]
        str_replace_all("\\(.*?\\)", "") %>% ## remove anything in parenthesis; assuming that author names are NOT in parentheses
        str_replace_all("\\<.*?\\>", "") %>% ## remove anything in <>
        str_replace_all(";", ",") %>% ## replace ; with whitespace
        str_replace_all(" - ", ",") %>%
        str_replace_all("/", ",") %>% ## replace ; with whitespace
        str_replace_all("&", ",") %>% ## eliminate &
        str_replace_all("^\\s+|\\s+$|\\s+(?=\\s)", "") %>% ## eliminate white space
        str_replace_all("\t+", " ") %>% ## replace tabs with whitespace
        str_replace_all("\\.", "") %>% ## eliminate .
        str_replace_all("\"|'", "") %>% ## eliminate quotes
        str_replace_all(" and ", ",") %>%
        str_replace_all(" AND ", ",") %>%
        str_replace_all("\\)", "") %>%
        str_replace_all("R port by ", "") %>%
        str_replace_all("with contributions from:|with the contributions from|with contributions of|with contributions from|with collaborations of|with collaborations of|with collaborations by|with contribution from|with contributions of|with substantial contributions of code by|With considerable contributions by|with contributions by|with contribution by|with contributons from|With contributions from|with considerable contribution from|with a contribution from|with a contribution of|with a code snipped borrowed from", ",") %>%
        str_replace_all("with additional code from|with code developed by the|with code for case-control data contributed by|with collaboration of|with corrections by|with embedded Fortran code due to|with help from|with ideas from|with loess code from|with some assistance from|with some Fortran code adapted by|from the original by|with support from|Based on earlier work by| with data provided by|with suggestions from|Zhejiang university school of medicine|with tsvq code originally from|with the colaboration of|with Fortran code for Sampson-Guttorp estimation authored by|with \\\\code\\{hwexact\\} from|Ported to R by|Originally written for S-Plus by:|functions from rastamat by|R functions by|contains copies of ttice functions written by|Datasets via|qrng functions by|We are grateful to|S functions written by|comments go to|compiled by|Compiled by|The included GUDHI is authored by", ",") %>%
        str_replace_all("Based on models developed by|BiSSE-ness by|for the|based in part on an earlier implementation by|based on code from|contribution of|contributions by|contributions from|Contributions from|Contributions by|Contribution from|Additional contributions|data collected by|Original|R version by|Enhancements by|also based on C-code developed by|Function simpls based on simplsfit by|apart from a set of Fortran-77 subroutines written by|assisted by|R code by|based on Onigmo by|based on original code by|R documentation provided by|Transfer Entropy Packge:| Additional Code by|the contents of this package were written by", ",") %>%
        str_replace_all("based on readdcf by|based on RSvgDevice by|based on the program by|based on the source code from the randomForest package by|based on the work of|Author:|Author|Function fbvpot by:|Function fbvpot by|based in part on C code written by|based on|under the supervision of|Special thanks are due to|Contains|functions from rastamat by|The included parts of the libmad MPEG audio decoder library are authored by|together with|see README Function getHostnameSystem from package Rutils by|Earlier developements by|Contributors:|Contributor:|S original by| adopted to recent S-PLUS by|S scripts originally by|with key contributors|some code modified from|some package testing by|Derived from mda:mars by|substantially revised from|MATLAB code which is in turn adopted from", ",") %>%
        str_replace_all("Includes R source code and|Significant contributions on the package skeleton creation|with code for the Fourier transform from the seewave package|with general advice from the R-help listserv community|with parts adapted from Fortran|also changed its un-safe pointer arithmetics|ANN Library:|are provided under the terms of the GNU General Public License|readxportR is adapted from the Hmisc package created by|R port + extensions by|zlib from| Uses |Fortran utilities with|Cards were created by|method implementation by|s\\@R: c|s\\@R: person|examples from the argparse Python module by the Python Software Foundation Ports examples from the getopt package by|transition to Plan 9 codebase by", ",") %>%
        str_replace_all("The package uses functions from dlib|others in each function manual|plotting functions|for histsu function|for qqglddefault function|readMP3 function from the tuneR package|provided creative direction|for significant work on the functions new to version 20: cstratapsa|authored the function mtxexp|whose code has been included in our source package|released into the public domain They were downloaded from http:|with support|for low discrepancy algorithm|leaps wrapper|for gld C codes|s of libhunspell|S original|MATLAB code|support from the French National Research Program for Environmental|Apache Commons Codec", " ") %>%
        str_replace_all("as represented by the Minister of Natural Resources Canada|as well as code fragments|assisted on the multicore|yale sparse matrix package authors|with GeoSSE|up to version 20|belonging to the suite Quadpack|for corrections|based on code written during 2005|based on datasets provided on the books website|based on the Matlab code of Royset|Funded by the National Institute on Drug Abuse Award number|is the sole author|documentation", " ") %>%
        str_replace_all("based on the S code in the XGobi distribution Windows port based on this|authors of included software See file AUTHORS for details|Authors of libhunspell|authors of lmgls|authors of R function lm|authors of the ARPACK library See file AUTHORS for details|authors of the java libraries|Authors\\@R: c|Authors\\@R: person|which is not on r-cran  The SCEoptim function is adapted|a method developed by|contributors of MsgPack|contributors of QuantLib|contributors of the included fonts See file AUTHORS for details|contributors of the included software See file AUTHORS for details", " ") %>%
        str_replace_all("contributors worldwide|United States Government as represented by the US Army Research Laboratory|copyright claimed by Steven L Scott is limited to modifications made to the original code|copyright notices have been maintained in all source files In these cases|Copyright: Regents of the University of California|CPU implementation|creator of the BOOM project Some code in the BOOM libraries has been modified from other open source projects These include Cephes|Cuba library has been written by|See AUTHORS file|see AUTHORS file for additional contributors|see COPYRIGHTS file|See file AUTHORS|Randall C Johnson are Copyright SAIC-Frederick Inc Funded by the Intramural Research Program", " ") %>%
        str_replace_all("a modified version of the R math libraries|about 22e5 survey participants|Alcoholism Award Number R03 AA019775|We removed standard IO related functions|mnormt package|Note that maintainers are not available to give advice on using a package they did not author|DePauw University|following earlier work|foreignh are copied or adapted from the R foreign package created|for numerous other suggestions|forked off of rPython|Fortran code|Fortran original|Fourier transform from the seewave package", " ") %>%
        str_replace_all("fprintf|Developer| -- Unless otherwise noted|him into the public domain: http:|Regularized random forest for regression|ibm2ieeec were extracted from BRL-CAD file|ideas from the former package sound|ieee2ibmc|included version of CppJieba|included version of Eigen|incorporates code from cddlib written|Interface to R was written|Iowa State University|with R port|Adapted to R2OpenBUGS from R2WinBUGS|for L moments codes|largely translated from the TTBOX Matlab toolbox|Leaflet contributors|Leaflet Providers contributors|Learned Pattern Similarity for time series|- Ferdowsi University Of Mashhad|bicubic\\* functions|The function tem is|Toplevel R functions|the S code in the XGobi distribution Windows port|The University of Edinburgh Excerpts adapted from   Copyright|their names are randomly ordered", " ") %>%
        str_replace_all("contains copies of ttice functions written by|contains copies of lattice functions written by|Copyright - United States Government as represented by the US Army Research Laboratory", "") %>%
        str_replace_all("\\d{4}", "") %>%
        str_replace_all("R core team|R Core Development Team|R Core team|R Core Team|R Development Core Team|R core|R Core Deveopment Team|R development team|R-core|R-Core|the R Core Team|The R Core Team|The R Core Team -", "R Core") %>%
        str_replace_all("the CRAN team|the CRAN Team", "CRAN Team") %>%
        str_replace_all("^The +|^the +", "") %>%
        str_replace_all("Wickham Hadley|Hadley Wickham function", "Hadley Wickham") %>%
        str_replace_all("Yihui Xie function", "Yihui Xie") %>%
        str_split(",") %>%
        lapply(function(x) unlist(str_replace_all(x, ",|^\\s+|\\s+$", ""))) ## eliminate remaining commas and leading and trailing whitespaces
}

