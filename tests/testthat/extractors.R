context("Test extractor functions")

package_db <- clean_CRAN_db()
package_network <- build_network(object = package_db)
author_network <- build_network(object = package_db, perspective = "author")

test_that("author_of returnes same value when applied to a package network and when applied to an author network",
          expect_equal(sort(author_of(package_network, "MASS")),
                       sort(author_of(author_network, "MASS")))
)

test_that("package_by returnes same value when applied to a package network and when applied to an author network",
          expect_equal(sort(package_by(package_network, "Achim")),
                       sort(package_by(author_network, "Achim")))
)

test_that("package_by with exact = TRUE returns correct results", {
          expect_equal(package_by(package_network, "Ioannis Kosmidis", exact = TRUE),
                       c("betareg", "brglm", "brglm2", "enrichwith", "PlackettLuce",
                         "profileModel", "trackeR"))
          expect_equal(package_by(author_network, "Ioannis Kosmidis", exact = TRUE),
                       c("betareg", "brglm", "brglm2", "enrichwith", "PlackettLuce",
                         "profileModel", "trackeR"))
}
)

test_that("author_of with exact = TRUE returns correct results", {
          expect_equal(author_of(package_network, "brglm", exact = TRUE),
                       "Ioannis Kosmidis")
          expect_equal(author_of(author_network, "brglm", exact = TRUE),
                       "Ioannis Kosmidis")
}
)

