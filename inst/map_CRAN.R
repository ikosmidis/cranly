cran_db <- tools::CRAN_package_db()
packages_db <- clean_CRAN_db(packages_db = cran_db)
package_network <- setup_cranly_network(object = packages_db)
author_network <- setup_cranly_network(object = packages_db, perspective = "author")

## Global author network statistics
author_summaries <- summary(author_network)
author_summaries %>% arrange(desc(page_rank)) %>% head(10)
author_summaries %>% arrange(desc(eigen_centrality)) %>% head(10)

## Global package network statistics
package_summaries <- summary(package_network)
package_summaries %>% arrange(desc(eigen_centrality)) %>% head(10)
package_summaries %>% arrange(desc(page_rank)) %>% head(10)
package_summaries %>% filter(package == "brglm2")

## Visualize my packages
my_packages <- subset(network$nodes, grepl("Ioannis Kosmidis", Author))$Package

visualize.cranly_network(package_network, packages = my_packages, physics_threshold = 500)
visualize.cranly_network(author_network, authors = "Ioannis Kosmidis", physics_threshold = 500)
