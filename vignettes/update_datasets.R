library("cranly")

cran_db <- clean_CRAN_db()
package_network <- build_network(cran_db)
author_network <- build_network(object = cran_db, perspective = "author")

usethis::use_data(cran_db, overwrite = TRUE)
usethis::use_data(package_network, overwrite = TRUE)
usethis::use_data(author_network, overwrite = TRUE)
