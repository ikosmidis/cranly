library("cranly")

cran_db <- clean_CRAN_db()
package_network <- build_network(cran_db)
author_network <- build_network(object = cran_db, perspective = "author")

saveRDS(cran_db, file = "data/cran_db.rds")
saveRDS(package_network, file = "data/package_network.rds")
saveRDS(author_network, file = "data/author_network.rds")

