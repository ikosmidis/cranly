# Copyright (C) 2018- Ioannis Kosmidis

#' Clean and organize package and author names in the output of [`tools::CRAN_package_db()`]
#'
#' @aliases cranly_db
#'
#' @param packages_db a [`data.frame`] with the same structure to the
#'     output of [`tools::CRAN_package_db`] (default) or
#'     [`utils::available.packages`].
#' @param clean_directives a function that transforms the contents of
#'     the various directives in the package descriptions to vectors
#'     of package names. Default is [`clean_up_directives`].
#' @param clean_author a function that transforms the contents of
#'     `Author` to vectors of package authors. Default is
#'     [`clean_up_author`].
#'
#' @details
#'
#' [`clean_CRAN_db`] uses [`clean_up_directives`] and
#' [`clean_up_author`] to clean up the author names and package names
#' in the various directives (like `Imports`, `Depends`, `Suggests`,
#' `Enhances`, `LinkingTo`) as in the [`data.frame`] that results from
#' [`tools::CRAN_package_db`]  return an organized `data.frame` of
#' class [`cranly_db`] that can be used for further analysis.
#'
#' The function tries hard to identify and eliminate mistakes in the
#' Author field of the description file, and extract a clean list of
#' only author names. The relevant operations are coded in the
#' [`clean_up_author`] function. Specifically, some references to
#' copyright holders had to go because they were contaminating the
#' list of authors (most are not necessary anyway, but that is a
#' different story...). The current version of [`clean_up_author`] is
#' far from best practice in using regex but it currently does a fair
#' job in cleaning up messy Author fields. It will be improving in
#' future versions.
#'
#' Custom clean-up functions can also be supplied via the
#' `clean_directives` and `clean_author` arguments.
#'
#' @return
#'
#' A [`data.frame`] with the same variables as `package_db` (but with
#' lower case names), that also inherits from `class_db`, and has a
#' `timestamp` attribute.
#'
#' @examples
#' \donttest{
#' ## Before cleaning
#' cran_db <- tools::CRAN_package_db()
#' cran_db[cran_db$Package == "weights", "Author"]
#'
#' ## After clean up
#' package_db <- clean_CRAN_db(cran_db)
#' package_db[package_db$package == "weights", "author"]
#' }
#' @export
clean_CRAN_db <- function(packages_db = tools::CRAN_package_db(),
                          clean_directives = clean_up_directives,
                          clean_author = clean_up_author) {

    if (is.matrix(packages_db)) {
        packages_db <- as.data.frame(packages_db)
    }

    md5 <- grepl("MD5sum", names(packages_db))
    if (any(md5)) {
        ## Remove redundant MD5 sum
        ind <- which(md5)
        if (length(ind) > 1) {
            packages_db <- packages_db[-ind[1]]
        }
    }
    else {
        warning("no MD5sum information found in package_db")
        packages_db$MD5sum <- NA
    }

    ## Remove duplicated packages
    packages_db <- packages_db[is.na(packages_db$Package) | !duplicated(packages_db$Package), ]

    if (is.null(packages_db$Author)) {
        warning("no author information found in package_db")
        packages_db$Author <- NA
    }
    if (is.null(packages_db$Date)) {
        warning("no date information found in package_db")
        packages_db$Date <- NA
    }
    if (is.null(packages_db$URL)) {
        warning("no url information found in package_db")
        packages_db$URL <- NA
    }
    if (is.null(packages_db$Maintainer)) {
        warning("no Maintainer information found in package_db")
        packages_db$Maintainer <- NA
    }


    ## Coerce variable names to lower case
    names(packages_db) <- tolower(names(packages_db))

    packages_db <- within(packages_db, {
        imports <- clean_directives(imports)
        depends <- clean_directives(depends)
        suggests <- clean_directives(suggests)
        enhances <- clean_directives(enhances)
        linking_to <- clean_directives(linkingto)
        reverse_imports <- clean_directives(`reverse imports`)
        reverse_depends <- clean_directives(`reverse depends`)
        reverse_suggests <- clean_directives(`reverse suggests`)
        reverse_enhances <- clean_directives(`reverse enhances`)
        reverse_linking_to <- clean_directives(`reverse linking to`)
        author <- clean_author(author)
        date <- as.Date(date)
        published <- as.Date(published)
    })

    ## Clean up
    packages_db["reverse depends"] <- packages_db["reverse imports"] <-
        packages_db["reverse suggests"] <- packages_db["reverse enhances"] <-
        packages_db["reverse linking to"] <- packages_db["linkingto"] <- NULL
    

    attr(packages_db, "timestamp") <- Sys.time()

    class(packages_db) <- c("cranly_db", class(packages_db))
    packages_db
}

#' Clean up package directives
#'
#' @param variable a character string.
#'
#' @return
#'
#' A list of one vector of character strings.
#'
#' @examples
#' clean_up_directives("R (234)\n stats (>0.01),     base\n graphics")
#' @export
clean_up_directives <- function(variable) {
    variable %>%
        str_replace_all("\n", ",") %>%
        str_replace_all("\\([^()]*\\)", "") %>%
        str_replace_all(" ", "") %>%
        ## Eliminate R from dependencies
        str_replace_all("\\bR,\\b", "") %>%
        str_replace_all("\\b,R\\b", "") %>%
        str_replace_all("^R$", "") %>%
        str_split(",") %>%
        lapply(function(x) {
            out <- str_replace_all(x, ",|^\\s+|\\s+$", "")
            out <- out[!(out == "")]
            if (all(is.na(out))) character(0) else out
        })## eliminate remaining commas and leading and trailing whitespaces
}

#' Clean up author names
#'
#' @param variable a character string.
#'
#' @return
#'
#' A list of one vector of character strings.
#'
#' @examples
#' clean_up_author(paste("The R Core team, Brian & with some assistance from Achim, Hadley;",
#'                       "Kurt\n Portugal; Ireland; Italy; Greece; Spain"))
#' @export
clean_up_author <- function(variable) {

    repl_symbols0 <- c(";", "/", "&", ":")
    ## "\\+"
    repl_symbols1 <- c(" - ", "'s", "\"|'",
                       "\\*", "\\(", "\\)")
    ## "\\." "\n", \t+

    repl_other_words0 <- c("and", "with", "by", "due", "from", "uses", "supervision of")
    repl_other_words1 <- c("also", "apart", "as", "at", "based", "itself",
                           "during", "each", "for", "him", "her", "in",
                           "into", "its", "not", "of", "off", "on",
                           "or", "otherwise", "our", "some", "that",
                           "their", "these", "they", "this", "to",
                           "then", "since", "etc", "except",
                           "together", "under", "unless", "up", "via",
                           "we", "which", "whose", "the", "both", "much", "more")
    ## "a"

    repl_nouns0 <- c("advice", "algorithm", "author", "coauthor", "book", "case", "name",
                     "guidance", "policy", "creation", "portion",
                     "code", "codebase", "collaboration", "colaboration", "comment",
                    "content", "contribution", "contributor",
                    "control", "copies", "copy", "copyright",
                    "correction", "creator", "data", "dataset",
                    "detail", "development", "developement", "direction",
                    "discrepancy", "distribution", "document",
                    "documentation", "domain", "earlier", "email",
                    "enhancement", "entropy", "estimation", "example",
                    "excerpt", "extension", "file", "font",
                    "fragment", "function", "guide", #"group",
                    "help", "idea",
                    "implementation", "library", "maintainer",
                    "manual", "math", "method", "model",
                    "modification", "module", "moment", "note",
                    "notice", "other", "package", "part",
                    "participant", "pattern", "person", "professor",
                    "program", "provider", "project", "script",
                    "series", "set", "similarity", "software",
                    "source", "subroutine", "routine", "suggestion",
                    "supervision", "survey", "term", "testing", "tests",
                    "thanks", "time", "tool", "toolbox", "transform",
                    "transition", "turn", "utilities", "utility",
                    "version", "website", "work", "worldwide",
                    "wrapper", "number", "libraries", "packge", "input",
                    "minister", "queen", "majesty", "right",
                    "resource", "webpage", "material",
                    "colleague") # add s
    repl_nouns1 <- paste0(repl_nouns0, "s")

    repl_adjectives0 <- c("additional", "all", "assistance", "initial",
                         "available", "considerable", "creative",
                         "former", "general", "grateful", "interface",
                         "key", "low", "new", "numerous", "open",
                         "original", "public", "recent",
                         "significant", "sole", "special", "standard",
                         "substantial", "support", "toplevel",
                         "unsafe", "written", "various", "natural",
                         "most", "many", "current")

    repl_verbs0 <- c("adapt", "adopt", "are", "assist", "author",
                     "debug", "debugging", "enhance", "extend",
                    "base", "belong", "borrow", "change", "claim",
                    "collect", "compile", "contain", "contribute",
                    "cover", "package", "inspire",
                    "create", "derive", "develop", "download",
                    "embed", "extract", "follow", "fork", "fund",
                    "give", "go", "have", "include", "incorporate",
                    "interface", "is", "learn", "limit", "maintain",
                    "modified", "modify", "note", "order", "plot", "plotting",
                    "port", "prepare", "provide", "publish", "relate",
                    "release", "remove", "represent", "revise", "see",
                    "set", "snip", "snipped", "transfer", "translate", "use",
                    "was", "were", "been", "copied", "did",
                    "embedded", "had", "made", "list", "comply") # Add d ed s ing (remove e if last)
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
                       "L-", "Python", "Perl", "Stata", "octave")#, "R", "S", "C")

    repl_software0 <- c("ars", "loess", "tsvq", "rastamat", "ttice", "gzstream", "PHAT",
                        "mars", "mda", "estout", "RDCOMClient", "GSL",
                        "ppexp", "es5-shim", "OpenBugs",
                        "Netlib", "NAG", "odepack", "LAPACK",
                        "qrng", "GUDHI", "BiSSE-ness", "simpls", "EISPACK",
                        "simplsfit", "Onigmo", "readdcf",
                        "RSvgDevice", "randomForest", "fbvpot",
                        "fbvpot", "rastamat", "libmad", "MPEG",
                        "audio", "decoder", "getHostnameSystem",
                        "Rutils", "mdamars", "seewave", "listserv",
                        "GNU", "General", "Public", "License", "iwidgets",
                        ## "ANN",
                        "readxportR", "Hmisc", "zlib", "Cards",
                        "argparse", "Python Software Foundation",
                        "getopt", "Plan 9", "dlib", "histsu",
                        "qqglddefault", "readMP3", "tuneR",
                        "cstratapsa", "mtxexp", "leaps", "gld",
                        "libhunspell", "Apache Commons Codec",
                        "multicore", "yale sparse matrix", "GeoSSE",
                        "suite Quadpack", "XGobi",
                        "libhunspell", "lmgls", "lm", "ARPACK",
                        "SCEoptim", "MsgPack", "QuantLib", "BOOM",
                        "Cephes", "Cuba", "mnormt", "foreignh",
                        "foreign", "rPython", "seewave", "fprintf",
                        "ibm2ieeec", "BRL-CAD", "sound", "ieee2ibmc",
                        "CppJieba", "Eigen", "cddlib", "R2OpenBUGS",
                        "R2WinBUGS", "TTBOX", "Leaflet", "XGobi",
                        "tem", "bicubuc", "bicubic", "ttice", "emgllmfitter",
                        "lattice", "pm", "NEWUOA", "ClaSSE", "LevelDB",
                        "ARMS", "DLNAC1", "DLARPC", "DLAPST", "ScaLAPACK",
                        "fdGPH", "fdSperio", "libuv", "starship", "GPC",
                        "onenormest", "mdm", "multinom", "nnet",
                        "Spectra", "eclat", "JMapViewer",
                        "CVODE", "SUNDIALS", "DEoptim", "DE-Engine", "h2",
                        "Moment.js contributors", "dchud", "dchdd", "printf",
                        "compresid2way",
                        "f.robftest",
                        "last",
                        "p.scales",
                        "p.dnorm",
                        "p.arrows",
                        "p.profileTraces",
                        "p.res.2x",
                        "histBxp",
                        "p.tachoPlot",
                        "KSd",
                        "ecdf.ksCI",
                        "cairoSwd",
                        "is.whole",
                        "toLatex.numeric",
                        "Duplicated",
                        "p.res.2fact",
                        "empty.dimnames",
                        "primes",
                        "inv.seq",
                        "loessDemo", "ANN Library",
                         "SLATEC Common Mathematical Library")#, "ALINE")

    repl_technical0 <- c("CPU", "Windows", "Mac", "README",
                         "Fourier", "http", "Linux",
                         "statlib", "CRAN", "github")

    repl_adverbs0 <- c("largely", "originally", "previously",
                       "randomly", "substantially", "equally",
                       "well", "partly", "partially", "potentially",
                       "especially")

    ## These are case-insensitive replacements to be done after replacing symbols (some cph go here)
    repl_replacements0 <- c("based on", "based on earlier work by")
    repl_replacements1 <- c("has", "DePauw University", "LUNAM Universite",
                            "Universite Angers", "Laboratoire d'ergonomie et d'epidemiologie en sante au travail",
                            "French National Research Program",
                            "Environmental and Occupational Health of Anses",
                           "Minister of Natural Resources Canada",
                           "National Institute on Drug Abuse Award",
                           "Zhejiang university",
                           "University of Pittsburgh",
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
                           "Agronomy Faculty",
                           "Missouri Botanical Garden",
                           "Province of British Columbia",
                           "Foundation SmarterPoland.pl",
                           "University Hospital of Zurich",
                           "Menne Biomed Consulting Tuebingen",
                           "Dep Gastroenterology",
                           "Pacific Climate Impacts Consortium",
                           "Students REU13",
                           "R Special Interest Group on Databases",
                           "University of Oxford",
                           "7th edition",
                           "CASTLE research group",
                           "University of Puerto Rico-Mayaguez",
                           "Stat.ASSQ",
                           "University of Tennessee Research\\s+Foundation",
                           "University of Tennessee",
                           "University of California Berkeley",
                           "University of Colorado Denver",
                           "Commonwealth of Australia AEC",
                           "University of Washington",
                           "Open Water Analytics",
                           "University of Canterbury",
                           "Crown Copyright 2018",
                           "CENSUR working survival group",
                           "TU Wien",
                           "other Node contributors",
                           "version 1.0",
                           "version 2.0",
                           "Federal University of Pernambuco",
                           "Federal Rural University of Pernambuco",
                           "jQuery UI",
                           "Novartis Institute for BioMedical Research",
                           "National Geodetic Survey",
                           "Berlin School of Economics and Law",
                           "Massachusetts Institute of Technology",
                           "AnacletoLab, Dipartimento di Informatica",
                           "Universita degli Studi di Milano",
                           "Charite, Universitatsmedizin Berlin",
                           "Integrative Analysis of Longitudinal Studies of Aging",
                           "University of Duisburg-Essen",
                           "Department of Psychology",
                           "Dalhousie University",
                           "The School of Business", "Portland State University",
                           "University of Copenhagen", "Kungliga Tekniska Hogskolan", "CWI",
                           "database design", "MuckRock Project",
                           "NAEP Primer", "R\\s+package",
                           "British Geological Survey", "New York University",
                           "University of Hamburg",
                           "National Institutes for Standards and Technology",
                           "Chipmunk BASIC", "Indiana University",
                           "Data Science Team", "Predictive Analytics Team",
                           "Pennsylvania State University", "University of Bristol",
                           "Department for Biogeochemical Integration at MPI-BGC, Jena, Germany",
                           "ILO Department of Statistics", "University of Waikato",
                           "University of Konstanz", "listserv community",
                           "University of Warwick",
                           "Institut de Radioprotection et de Surete Nucleaire",
                           "Southern Methodist University", "Lawrence Livermore National Security",
                           "Department of Biostatistics",
                           "University of Texas", "Data Science Workshops", "Umea University",
                           "Department of Statistics", "Geometry Center", "University of Minnesota",
                           "Institute of Formal and Applied Linguistics",
                           "Charles University in Prague", "Czech Republic",
                           "THL A29 Limited", "Research Applications Laboratory",
                           "Valid International office@validinternational.org",
                           "Water and Sanitation for the Urban Poor",
                           "Population Division, Department of Economic and Social Affairs",
                           "Aalto University", "American University", "International-Harvard Statistical Consulting Company",
                           "Brazilian Jurimetrics Association", "IQSS Harvard University", "Johns Hopkins University", "Lund University", "Ohio State University", "Universidad Nacional de Colombia",
                           "Universidade Federal de Santa Maria", "Universidad de Chile",
                           "Universite Paris Dauphine", "Valid International", "Vanderbilt University")

    ## These are case-insensitive replacements to be done first
    repl000 <- c("other code", "Engineering and Physical Sciences Research Council",
                 "simpls\\.fit", "\\.f", "\\bS\\b\\s+original", "ld98",
                 "S-Plus\\s+original", "\\\\code\\{hwexact\\(\\)\\}",
                 "Ported\\s+to\\s+R", "R\\s+port", "University of Perugia",
                 "R\\s+code", "C\\s+code[s ]",  "Decision Patterns",
                 "Free Software Foundation, Inc", "U.S. NIST", "lm.gls",
                 "Free Software Foundation", "mtx\\.exp",
                 "U.Washington, Seattle", "C-code",
                 "jQuery contributors", "MathQuill contributors",
                 "a\n\tpackage", "Ph\\.D\\.", "PhD", "R\\s+source",
                 "good questions", "\\\\email\\{bmarx\\@LSU.EDU\\}", "\\\\email\\{mgallo\\@unior.it\\}",
                 "\\\\email\\{paul.conn\\@\\@noaa.gov\\}",
                 "Very Good Research & Development, LLC",
                 "The Jackson Laboratory for Genomic Medicine, Farmington CT, USA",
                 "NIH, National Cancer Institute",
                 "Center for\\s+Cancer Research under NCI Contract NO1-CO-12400",
                 "D3 contributors", "Vega contributors", "ks.gof",
                 "qqgld.default", "SAIC-Frederick, Inc",
                 "WG127 SCOR/IAPSO",
                 "The students of the `Advanced R Programming Course'",
                 "Scientific Software Development", "www.linhi.com",
                 "Geospatial Information Authority of Japan",
                 "Joint Research\\s+Centre of the European Commission",
                 "Press et al",
                 "National Institute on Drug Abuse", "Award number 1R03DA030850", "National Institute on Alcohol Abuse and Alcoholism", "Award Number R03 AA019775", "National Institute of Justice", "Award Number 2011-RY-BX-0003",
                 "Statistical Consulting - intelligent data analysis, stochastic modelling and statistical inference", "'sumtxt'", "Dept. of Integrative Biology", "U.C. Berkeley",
                 "MonetDB B.V.", "http://meteo.unican.es",
                 "http://www.itl.nist.gov/div898/strd/nls/nls_main.shtml",
                 "version 0.1-13", "Python Team", "NumPy team", "IRD CEPED",
                 "College of William \\& Mary", "read.dcf",
                 "cstrata.psa", "cv.bal.psa", "cv.trans.psa", "Knuth-TAOCP RNG", "underlying SFMT",
                 "Unit D.02 Water and Marine Resources", "UMS RIATE", "QOS.ch",
                 "IP2Location.com", "Regularized random forest for classification",
                 "ERC Grant Agreement number 336167 - the CHAI Project",
                 "http://code.google.com/p/vector-playing-cards/downloads/detail\\?name=PNG-cards-1.2.zip",
                 "Rice University's DSP group", "e\\.g\\.", "version 2.1", " version 2.2", "v 4.6-7 for SNPs",
                 "MD Anderson Cancer\\s+Center", "Laboclima - Universidade Federal do Paran",
                 "Faculty of Mathematics and\\s+Physics", "CEPOI - EA 7388", "about 2.2e5",
                 "Fordham University, NY", "INRIA-Chile", "Universite Montpellier 2", "Universite Paris Dauphine")

    repl_lists0 <- "R-help"

    ## Remove hopeless entries
    inds <- grepl("^Authors\\@R|^c\\(|^\\s+c\\(", variable)
    variable[inds] <- ""

    countries <- countrycode::codelist$country.name.en
    countries <- countries[-which(countries %in% c("Jordan"))] ## exclude country names here

    variable %>%
        split_and_replace(repl000, "", fixed_word = FALSE) %>%
        str_replace_all(", Inc", " Inc") %>%
        str_replace_all(", Ltd", " Ltd") %>%
        str_replace_all(", LLC", " LLC") %>%
        str_replace_all("\\[(.|\n)*?\\]", "") %>% ## remove all [aut, ctb etc]
        str_replace_all("\\((.|\n)*?\\)", "") %>% ## remove anything in parenthesis; assuming that author names are NOT in parentheses
        str_replace_all("\\<(.|\n)*?\\>", "") %>% ## remove anything in <>
        str_replace_all("\\.", " ") %>%
        str_replace_all("\n", " ") %>%
        str_replace_all("\t+", " ") %>%
        str_replace_all("R\\s+Core\\s+Deveopment\\s+Team|R(-|\\s+)[Cc]ore\\s+[Tt]eam|R\\s+[Dd]evelopment\\s+[Tt]eam|R(-|\\s+)[Cc]ore\\s+[Dd]evelopment\\s+[Tt]eam|R\\s+core|R(-|\\s+)[Cc]ore|R(-|\\s+)[Dd]evelopment\\s+[Cc]ore\\s+[Tt]eam", "R Core") %>%
        split_and_replace(repl_lists0, "") %>%
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
        str_replace_all("\\ba\\b|\\ban\\b", "") %>% # replace a but not A
        split_and_replace(repl_nouns0, "") %>%
        split_and_replace(repl_nouns1, "") %>%
        split_and_replace(repl_adjectives0, "") %>%
        split_and_replace(repl_verbs0, "") %>%
        split_and_replace(repl_verbs1, "") %>%
        split_and_replace(repl_verbs2, "") %>%
        split_and_replace(repl_verbs3, "") %>%
        split_and_replace(repl_technical0, "") %>%
        split_and_replace(repl_adverbs0, "") %>%
        split_and_replace(countries, "") %>%
        str_replace_all("\\d{4}-\\d{4}", "") %>%
        str_replace_all("\\d{4}", "") %>%
        str_replace_all("\\d{3}", "") %>%
        str_replace_all("\\d{2}", "") %>%
        str_replace_all("[Cc][Rr][Aa][Nn] [Tt]eam", "CRAN Team") %>%
        str_replace_all("R[ sS]tudio|R[ sS]tudio Inc", "RStudio") %>%
        str_replace_all("^H2O ai team$|^H2O ai$", "H2O.ai") %>%
        ## Special people
        str_replace_all("Wickham Hadley|Hadley Wickham function", "Hadley Wickham") %>%
        str_replace_all("Yihui Xie function", "Yihui Xie") %>%
        str_replace_all("B D Ripley|Brian D Ripley", "Brian Ripley") %>%
        ## iconv(from = "UTF8", to = "ASCII//TRANSLIT") %>%
        str_split(",") %>%
        lapply(function(x) {
            x <- str_replace_all(x, ",|^\\s+|\\s+$|\\s+(?=\\s)", "")
            unique(x[!(x == "" | x == " " | x %in% letters | x %in% LETTERS | x == "-")])
        })

}

## if it starts with A do not remove A
## If it ends with R, S, A remove R
## If there is a programmer named "R", "S", "A" remove
## Remove single letters
## Clean sequences of single letters (e.g. "Martell-Juarez, D.A. & Nieto-Barajas, L.E.")
##\n replace with , or ""? "J.A. e.g. "Torres-Matallana [aut, cre]\n    K. Klepiszewski [aut, cre]\n    U. Leopold [ctb]\n    G. Schutz [ctb]\n    G.B.M. Heuvelink [ctb]"


