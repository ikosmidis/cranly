library("stringr")
library("tools")
library("tibble")
library("dplyr")
library("tidyr")
library("colorspace")
library("visNetwork")
library("tidygraph")
library("ggraph")
library("igraph")
library("Matrix")

cran_db <- CRAN_package_db()

packages_db <- prepare_CRAN_package_db(cran_db)

network <- prepare_CRAN_network(packages_db)

my_packages <- network[[2]] %>% filter(grepl("Ioannis Kosmidis", Author)) %>% select(Package) %>% unlist()

visualize.cranly_db_network(network, packages = my_packages, physics_threshold = 500)

gg1 <- graph.edgelist(network[[1]] %>% group_by() %>% select(from, to) %>% filter(from != "") %>% as.matrix %>% na.omit())
gg2 <- decompose.graph(gg1)
gg3 <- gg2[[which(sapply(gg2, vcount) == max(sapply(gg2, vcount)))]]

library(rgexf)

dd <- igraph.to.gexf(gg1)

write.gexf()




## Analytics
## Network summaries
## Difference colours for suggests, depends and enhances
## Add licence information












## Modellig
library("BradleyTerryScalable")
library("Matrix.utils")
paired_CRAN <- edges_CRAN %>%
    filter(type %in% c("import")) %>%
    transmute(winner = from, loser = to, n = 1) %>%
    na.omit()

bt_paired_CRAN <- btdata(paired_CRAN)

fit <- btfit(bt_paired_CRAN, a  = 2)

## Circle plot with bundled edges
edgebundle(net)

## Using graphjs
library(threejs)
library(htmlwidgets)

graphjs(net, vertex.size = 0.1)

## With cross talk so that one can select
