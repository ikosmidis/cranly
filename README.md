<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build
Status](https://travis-ci.org/ikosmidis/cranly.svg?branch=master)](https://travis-ci.org/ikosmidis/cranly)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ikosmidis/cranly/master.svg)](https://codecov.io/github/ikosmidis/cranly?branch=master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/cranly)](https://cran.r-project.org/package=cranly)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

cranly
======

[**cranly**](https://github.com/ikosmidis/cranly) provides core
visualisations and summaries for the CRAN package database. It is aimed
mainly as an analytics tool for developers to keep track of their CRAN
packages and profiles, as well as those of others, which, at least for
me, is proving harder and harder with the rapid growth of the
[CRAN](https://cran.r-project.org) ecosystem.

The package provides methods for cleaning up and organising the
information in the CRAN package database, for building package
directives networks (depends, imports, suggests, enhances, linking to)
and collaboration networks, and for computing summaries and producing
interactive visualisations from the resulting networks. Network
visualisation is through the
[**visNetwork**](https://CRAN.R-project.org/package=visNetwork) package.
The package also provides functions to coerce the networks to
[igraph](https://CRAN.R-project.org/package=igraph) objects for further
analyses and modelling.

### Installation

Install the development version from github:

``` r
# install.packages("devtools")
devtools::install_github("ikosmidis/cranly")
```

### Collaboration and package directives networks in CRAN

The first step in the **cranly** workflow is to try and “clean-up” the
package and author names in the data frame that results from a call to
`tools::CRAN_package_db()`

``` r
p_db <- tools::CRAN_package_db()
package_db <- clean_CRAN_db(p_db)
```

The CRAN database we use is from

``` r
attr(package_db, "timestamp")
#> [1] "2018-05-21 10:14:23 BST"
```

#### Package directives networks

The package directives network can then be built using

``` r
package_network <- build_network(package_db)
```

`package_network` can then be interrogated using extractor methods (see,
`?package_by`). For example, my packages can be extracted as follows

``` r
my_packages <- package_by(package_network, "Ioannis Kosmidis")
my_packages
#> [1] "betareg"      "brglm"        "brglm2"       "cranly"
#> [5] "enrichwith"   "PlackettLuce" "profileModel" "trackeR"
```

and their sub-network of directives can be summarized in an interactive
visualization, a shapshot of which is below

``` r
plot(package_network, package = my_packages, title = TRUE, legend = TRUE)
```

![](README_files/README-unnamed-chunk-6-1.png)

We can also compute package summaries and plot “Top-n” lists according
to the various summaries

``` r
package_summaries <- summary(package_network)
#> Warning in closeness(cranly_graph, normalized = FALSE): At centrality.c:
#> 2784 :closeness centrality is not well-defined for disconnected graphs
plot(package_summaries, according_to = "n_imported_by", top = 20)
```

![](README_files/README-unnamed-chunk-7-1.png)

``` r
plot(package_summaries, according_to = "page_rank", top = 20)
```

![](README_files/README-unnamed-chunk-7-2.png)

#### Collaboration networks

The collaboration network can also be built using a similar call

``` r
author_network <- build_network(package_db, perspective = "author")
```

and the extractor functions work exactly as they did for the package
directives network. For example, my collaboration network results can be
summarized as an interactive visualization, a shapshot of which is below

``` r
plot(author_network, author = "Ioannis Kosmidis")
```

![](README_files/README-unnamed-chunk-9-1.png)

“Top-n” collaborators according to various summaries can again be
computed

``` r
author_summaries <- summary(author_network)
#> Warning in closeness(cranly_graph, normalized = FALSE): At centrality.c:
#> 2784 :closeness centrality is not well-defined for disconnected graphs
plot(author_summaries, according_to = "n_collaborators", top = 20)
```

![](README_files/README-unnamed-chunk-10-1.png)

``` r
plot(author_summaries, according_to = "n_packages", top = 20)
```

![](README_files/README-unnamed-chunk-10-2.png)

``` r
plot(author_summaries, according_to = "page_rank", top = 20)
```

![](README_files/README-unnamed-chunk-10-3.png)

Well, the usual suspects…

#### Package dependence trees

Since version 0.2 **cranly** includes functions for constructing and
working with package dependence tree objects. A package’s dependence
tree shows what else needs to be installed with the package in an empty
package library with the package, and hence it can be used to + remove
unnecessary dependencies that “drag” with them all sorts of other
packages + identify packages that are heavy for the CRAN mirrors +
produced some neat visuals for the package

For example, the dependence tree of the **PlackettLuce** R package I am
co-authoring is

``` r
PL_dependence_tree <- build_dependence_tree(package_network, "PlackettLuce")
plot(PL_dependence_tree)
```

![](README_files/README-unnamed-chunk-11-1.png)

**cranly** also implements a *package dependence index* (see
?summary.cranly\_dependence\_tree for mathematical details). The closer
that is to 0 the “lighter” the package is

``` r
summary(PL_dependence_tree)
#> $package
#> [1] "PlackettLuce"
#>
#> $n_generations
#> [1] 3
#>
#> $parents
#> [1] "igraph"      "MASS"        "Matrix"      "partykit"    "psychotools"
#> [6] "psychotree"  "qvcalc"      "rARPACK"     "sandwich"
#>
#> $dependence_index
#> [1] 0.4177529
```

Check the package vignettes for a more comprehensive tour of the package
and for network visualisations on authors with orders of magnitude
larger collaboration networks than mine.

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
