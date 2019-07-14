context("Test extractor functions")


# cran_db <- clean_CRAN_db()
# package_network <- build_network(object = cran_db)
# author_network <- build_network(object = cran_db, perspective = "author")

# data("cran_db_today", package = "cranly")
data("pkg_net_today", package = "cranly")
data("aut_net_today", package = "cranly")

package_network <- pkg_net_today
author_network <- aut_net_today


test_that("author_of returns same value when applied to a package network and when applied to an author network", {
  expect_equal(
    sort(author_of(package_network, "MASS", exact = TRUE)),
    sort(author_of(author_network, "MASS", exact = TRUE))
  )
})

test_that("package_by with exact = TRUE returns correct results", {
  expect_true(all(package_by(package_network, "Ioannis Kosmidis", exact = TRUE) %in%
    c(
      "betareg", "brglm", "brglm2", "cranly", "enrichwith", "PlackettLuce",
      "profileModel", "semnar", "trackeR", "trackeRapp"
    )))
})

test_that("package_with with exact = FALSE returns correct results", {
    expect_true(all(c("biglm", "brglm", "brglm2") %in% package_with(package_network, "glm", exact = FALSE)))
    expect_true(all(package_with(author_network, "brglm", exact = FALSE) %in% c("brglm", "mbrglm", "brglm2")))
    expect_true(all(package_with(package_network, "brglm", exact = FALSE) %in% c("brglm", "mbrglm", "brglm2")))
    expect_identical(length(package_with(package_network, Inf, exact = FALSE, flat = TRUE)), 14995L)
    expect_identical(dim(package_with(package_network, Inf, exact = FALSE, flat = FALSE)), c(14995L, 65L))
    expect_identical(dim(package_with(author_network, Inf, exact = FALSE, flat = FALSE)), c(20373L, 2L))
})


test_that("author_of with exact = TRUE returns correct results", {
    expect_true("Ioannis Kosmidis" %in% author_of(package_network, "PlackettLuce", exact = TRUE))
})

test_that("author_with returns correct resuts", {
    expect_true(all(
        author_with(package_network, "Ioan") %in%
        c("Alex Ioannides", "Ioannis N Athanasiadis", "Ioannis Tsamardinos",
          "Ioanna Manolopoulou", "Lazaros Ioannidis", "Ioannis Kosmidis",
          "Eleni Ioanna Delatola", "Ioana-Elena Oana")))
    expect_true(all(
        author_with(author_network, "Ioan") %in%
        c("Alex Ioannides", "Ioannis N Athanasiadis", "Ioannis Tsamardinos",
          "Ioanna Manolopoulou", "Lazaros Ioannidis", "Ioannis Kosmidis",
          "Eleni Ioanna Delatola", "Ioana-Elena Oana")))
    expect_identical(length(author_with(package_network, Inf)), 20373L)
    expect_identical(length(author_with(author_network, Inf)), 20373L)
    expect_identical(dim(author_with(author_network, Inf, flat = FALSE)), c(20373L, 2L))
    expect_identical(dim(author_with(package_network, Inf, flat = FALSE)), c(14995L, 65L))
})

test_that("suggets, imports, linking_to works", {
  expect_true(all(suggests(package_network, "cranly", exact = TRUE) == c("knitr", "rmarkdown")))
  expect_equal(
    imports(package_network, "cranly", exact = TRUE),
    c(
      "visNetwork", "colorspace", "igraph", "magrittr", "stringr",
      "ggplot2", "countrycode"
    )
  )
  expect_equal(
    linking_to(package_network, "RcppArmadillo", exact = TRUE),
    "Rcpp"
  )
})

test_that("maintainer_of and maintained_by work", {
  expect_equal(maintainer_of(package_network, "cranly", exact = TRUE), "Ioannis Kosmidis")
  expect_equal(
    maintainer_of(package_network, c("brglm", "PlackettLuce"), exact = TRUE),
    c("Ioannis Kosmidis", "Heather Turner")
  )
  expect_equal(
    maintainer_of(package_network, c("brglm", "PlackettLuce"), exact = FALSE),
    c("Ioannis Kosmidis", "Euloge C Kenne Pagui", "Heather Turner")
  )
  expect_true(all(maintained_by(package_network, "Ioannis Kosmidis", exact = TRUE) %in%
    c(
      "betareg", "brglm", "brglm2", "cranly", "enrichwith", "PlackettLuce",
      "semnar", "profileModel", "trackeR", "trackeRapp"
    )))
  expect_error(maintained_by(author_network, "Ioannis Kosmidis", exact = TRUE))
  expect_error(maintainer_of(author_network, "brglm2", exact = TRUE))
})

test_that("email_of and email_with work", {
  expect_identical(email_of(package_network, "Ioannis Kosmidis"), "ioannis.kosmidis@warwick.ac.uk")
  expect_identical(nrow(email_of(package_network, "Heather Turner", flat = FALSE)), 4L)
  expect_true(all(email_with(package_network, c("kosmidis", "zeile")) %in%
    c("ioannis.kosmidis@warwick.ac.uk", "Achim.Zeileis@R-project.org")))
})


test_that("version_of and release_date_of", {
  expect_identical(version_of(package_network, "brglm2"), "0.5.1")
  expect_true(all(version_of(package_network, c("brglm2", "MASS")) %in%
    c("2.1.1", "1.0.3", "0.5.1", "1.0", "7.3-51.4", "1.1.0", "1.3", "0.5-3")))
})

test_that("title_of works as expected", {
  expect_identical(title_of(package_network, "semnar"), "Constructing and Interacting with Databases of Presentations")
  expect_identical(nrow(title_of(package_network, c("semnar", "brglm"), flat = FALSE)), 4L)
})

test_that("title_of works as expected", {
  expect_identical(title_of(package_network, "semnar"), "Constructing and Interacting with Databases of Presentations")
  expect_identical(nrow(title_of(package_network, c("semnar", "brglm"), flat = FALSE)), 4L)
})

test_that("release_date_of works as expected", {
  expect_identical(release_date_of(package_network, "semnar"), as.Date("2019-07-03"))
})
