#' Cleaned and processed CRAN package metadata from 20 March 2018
#'
#' This is a processed version of the result of a
#' \code{\link[tools]{CRAN_package_db}()} call on 20 March
#' 2018. Specifically, the resulting data frame was passed to the
#' \code{\link{clean_CRAN_db}} function and only the following
#' variables have been kept:
#' \itemize{
#'   \item package
#'   \item version
#'   \item date
#'   \item url
#'   \item depends
#'   \item imports
#'   \item suggests
#'   \item enhances
#'   \item license
#'   \item author
#'   \item maintainer
#'   \item contact
#'   \item md5sum
#' }
#'
#' The author names have been converted to ASCII using transliteration
#' through the \code{\link{iconv}} function.
#'
#' @docType data
#' @keywords datasets
#' @usage data("cran20032018", package = "cranly")
"cran20032018"
