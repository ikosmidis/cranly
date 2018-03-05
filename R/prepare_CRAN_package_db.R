#' @export
#' @import stringr tools tibble dplyr tidyr colorspace visNetwork
prepare_CRAN_package_db <- function(packages_db = CRAN_package_db(),
                                    clean_directives = clean_up_directives,
                                    clean_author = clean_up_author) {

    ## Remove reducndant MD5 sum
    packages_db <- as.tibble(packages_db[-which(grepl("MD5sum", names(packages_db)))[1]])

    ## Remove duplicated pacakges
    packages_db <- packages_db[!duplicated(packages_db$MD5sum), ]

    out <- packages_db %>%
        mutate(Imports = clean_directives(Imports),
               Depends = clean_directives(Depends),
               Suggests = clean_directives(Suggests),
               Enhances = clean_directives(Enhances),
               Author = clean_author(Author))

    attr(out, "timestamp") <- Sys.time()

    class(out) <- c("cranly_db", class(out))
    out
}
