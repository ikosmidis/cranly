context("Test extractor functions")

cran_db <- clean_CRAN_db()
package_network <- build_network(object = cran_db)
author_network <- build_network(object = cran_db, perspective = "author")

test_that("author_of returnes same value when applied to a package network and when applied to an author network",
          expect_equal(sort(author_of(package_network, "MASS")),
                       sort(author_of(author_network, "MASS")))
)

test_that("package_by with exact = TRUE returns correct results", {
          expect_true(all(package_by(package_network, "Ioannis Kosmidis", exact = TRUE) %in%
                       c("betareg", "brglm", "brglm2", "enrichwith", "PlackettLuce",
                         "profileModel", "trackeR")))
          expect_true(all(package_by(author_network, "Ioannis Kosmidis", exact = TRUE) %in%
                       c("betareg", "brglm", "brglm2", "enrichwith", "PlackettLuce",
                         "profileModel", "trackeR")))
}
)

test_that("author_of with exact = TRUE returns correct results", {
          expect_true("Ioannis Kosmidis" %in% author_of(package_network, "PlackettLuce", exact = TRUE))
}
)

