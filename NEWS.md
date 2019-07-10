# cranly 0.5

## Other improvements, updates and addition
* Moved to roxygen2 with rmarkdown
* Major documentation updates
* New improved codebased for extractor functions (see `?extractor-functions`)
* `date` and `published` in `cranly_db` objects are now of class `Date`
* `cranly_network_objects` with `perspective = "package` carry maintainer and maintainer email information


## New functionality
* New extractor functions: `maintainer_of`, `maintained_by`, `email_of`, `email_with`, `description_of`, `title_of`, `license_of`, `version_of`, `release_date_of` (see `?extractor-functions`)

# cranly 0.3

## Bug fixes
* Fixed a minor bug that may return duplicated base packages

## Other improvements, updates and addition
* Improved vignettes and fixed check erorrs that resulted in archiving on 2019-01-23
* Improved documentsation
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



