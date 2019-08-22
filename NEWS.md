# cranly 0.5.3

## Bug fixes
* Fixed a bug that would not allow `clean_CRAN_db` to work with the output of `utils::available.packages`
* Fixed a bug that would cause a warning about invalid factor levels in `build_network`

## Other improvements, updates and additions
* Fixed typos in news
* Improved the computational efficiency of `build_network`
* Improved the variables checks in `clean_CRAN_db`

# cranly 0.5.2

## New functionality
* New extractor functions `suggested_by`, `imported_by`, `dependency_of`, `linked_by`, `enhanced_by`, `suggesting`, `importing`, `linking_to`, `enhancing`, `maintainer_of`, `maintained_by`, `email_of`, `email_with`, `description_of`, `title_of`, `license_of`, `version_of`, `release_date_of`
* Plural aliases for extractor functions (e.g. `authors_of`, `release_dates_of` and so on)
* methods for word clouds of titles, description files and author names (see `?word_cloud`)
* `subset.cranly_network` now has a `maintainer` argument to subset by maintainer

## Bug fixes
* Fixed a bug where packages with no directives were removed from the network when `perspective = "package"` 
* Fixed extractor bugs and improved documentation
* `build_network` now works without explicitly supplying any arguments

## Other improvements, updates and additions
* Moved to [roxygen2 with rmarkdown](https://cran.r-project.org/package=roxygen2/vignettes/markdown.html)
* Major documentation updates
* New improved codebase for extractor functions (see `?extractor-functions`)
* `date` and `published` in `cranly_db` objects are now of class `Date`
* extractor functions have a new argument `flat`; if \code{FALSE} then a `data.frame` with all available information on the corresponding matches is returned
* `cranly_network_objects` with `perspective = "package` carry maintainer and maintainer email information
* New vignette on extractor functions and word clouds
* Updated DESCRIPTION file

# cranly 0.3

## Bug fixes
* Fixed a minor bug that may return duplicated base packages

## Other improvements, updates and addition
* Improved vignettes and fixed check errors that resulted in archiving on 2019-01-23
* Improved documentation
* Removed **testthat** from Suggests in DESCRIPTION

# cranly 0.2

## Bug fixes
* handling of duplicated packages in `clean_CRAN_db`
* various regex improvements
* Better handling of packages with . in their names

## New functionality
* `clean_CRAN_db` now accepts the matrix from `available.packages`
* new extractors for `cranly_network` objects: `suggests`, `depends`, `linking_to`, `imports`
* `compute_dependence_tree` is a recursion to compute all generations of a package (i.e. what else is installed)
* `build_dependence_tree` and a plot method for extracting the dependence tree of a package
* `summary` method for `cranly_dependence_tree` tree objects, reporting package dependence index, parents and depth

## Other improvements, updates and addition
* Added support for LinkingTo (thanks to Dirk Eddelbuettel for raising the [issue](https://github.com/ikosmidis/cranly/issues/1))
* Added acknowledgement to Turing Institute
* `plot` methods return legend and title by default


# cranly 0.1

* First public release



