library("cranly")

cran_db <- clean_CRAN_db()
package_network <- build_network(cran_db)
author_network <- build_network(object = cran_db, perspective = "author")

save(cran_db, file = "data/cran_db.rda")
save(package_network, file = "data/package_network.rda")
save(author_network, file = "data/author_network.rda")

