context("Test extractor functions")

cran_db <- clean_CRAN_db()
package_network <- build_network(object = cran_db)
author_network <- build_network(object = cran_db, perspective = "author")

test_that("author_of returnes same value when applied to a package network and when applied to an author network",
          expect_equal(sort(author_of(package_network, "MASS", exact = TRUE)),
                       sort(author_of(author_network, "MASS", exact = TRUE)))
)

test_that("package_by with exact = TRUE returns correct results", {
          expect_true(all(package_by(package_network, "Ioannis Kosmidis", exact = TRUE) %in%
                       c("betareg", "brglm", "brglm2", "cranly", "enrichwith", "PlackettLuce",
                         "profileModel", "trackeR", "trackeRapp")))
          expect_true(all(package_by(author_network, "Ioannis Kosmidis", exact = TRUE) %in%
                       c("betareg", "brglm", "brglm2", "cranly", "enrichwith", "PlackettLuce",
                         "profileModel", "trackeR", "trackeRapp")))
}
)

test_that("author_of with exact = TRUE returns correct results", {
          expect_true("Ioannis Kosmidis" %in% author_of(package_network, "PlackettLuce", exact = TRUE))
}
)

test_that("suggets, imports, linking_to works", {
    expect_true(all(suggests(package_network, "cranly", exact = TRUE) == c("knitr", "rmarkdown")))
    expect_equal(imports(package_network, "cranly", exact = TRUE),
                 c("visNetwork", "colorspace", "igraph", "magrittr", "stringr",
                   "ggplot2", "countrycode"))
    expect_equal(linking_to(package_network, "RcppArmadillo", exact = TRUE),
                 "Rcpp")
})

test_that("maintainer_of and maintained_by work", {
    expect_equal(maintainer_of(package_network, "cranly", exact = TRUE), "Ioannis Kosmidis")
    expect_true(all(maintained_by(package_network, "Ioannis Kosmidis", exact = TRUE) %in%
                    c("betareg", "brglm", "brglm2", "cranly", "enrichwith", "PlackettLuce",
                  "profileModel", "trackeR", "trackeRapp")))
    expect_error(maintained_by(author_network, "Ioannis Kosmidis", exact = TRUE))
    expect_error(maintainer_of(author_network, "brglm2", exact = TRUE))
})
