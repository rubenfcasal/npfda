#····································································
#   npfda_package.R
#····································································
#
#   Authors: Ruben Fernandez-Casal, Miguel Flores
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································
# pkgdown::build_site(getwd())

#····································································
# npfda-package ----
#····································································
#' npfda: Nonparametric functional data analysis
#'
#' This package implements nonparametric methods for inference
#' on (multidimensional) functional data using the tools available in package
#' [`npsp`](https://rubenfcasal.github.io/npsp).
#'
#' For more information visit <https://rubenfcasal.github.io/npfda.html>.
#' @keywords nonparametric spatial bootstrap Monte-Carlo
#' @name npfda-package
#' @aliases npfda
#' @docType package
#' @import npsp
#' @import graphics
#' @import stats
#' @importFrom grDevices rainbow
#' @references
#' Fernández-Casal R. (2023) npsp: Nonparametric spatial (geo)statistics.
#' R package version 0.7-11, <https://rubenfcasal.github.io/npsp>.
#'
#' Fernández-Casal R., Castillo-Páez S. and García-Soidán P. (2017), Nonparametric
#' estimation of the small-scale variability of heteroscedastic spatial processes,
#' *Spa. Sta.*, **22**, 358-370, [DOI](https://doi.org/10.1016/j.spasta.2017.04.001).
NULL


#····································································
# ozone ----
#····································································
#' Ground-level ozone
#'
#' Daily averages of ozone concentration (microgram per cubic meter) recorded over
#' the period from 1988 to 2020 at the Yarner Wood [AURN](https://uk-air.defra.gov.uk/networks/network-info?view=aurn)
#' monitoring site in the UK.
#'
#' It is assumed that the observations corresponding to each year are (partial)
#' realizations of a functional process, so the data consist of 33 curves observed
#' at 365 discretization points.
#' @source
#' The UK Automatic Urban and Rural Network ([AURN](https://uk-air.defra.gov.uk/networks/network-info?view=aurn)).
#' The data were downloaded from \url{https://uk-air.defra.gov.uk/data} and preprocessed
#' with the [`climatol`](https://climatol.eu/) package (by applying the usual outlier
#' detection and data imputation methods).
#' @name ozone
#' @docType data
#' @format A matrix with 365 rows (days) and 33 columns (years).
#' @keywords datasets
#' @examples
#' fd.ozone <- npf.data(ozone, dimnames = "day")
#' plot(fd.ozone)
NULL
# load("ozono.RData")
# ozone <- as.matrix(ozono[-1])
# colnames(ozone) <- 1988:2020
# rownames(ozone) <- 1:365
# usethis::use_data(ozone)


#--------------------------------------------------------------------
.onAttach <- function(libname, pkgname){
  #--------------------------------------------------------------------
  #   pkg.info <- utils::packageDescription(pkgname, libname, fields = c("Title", "Version", "Date"))
  pkg.info <- drop( read.dcf( file = system.file("DESCRIPTION", package = "npfda"),
                              fields = c("Title", "Version", "Date") ))
  packageStartupMessage(
    paste0(" npfda: ", pkg.info["Title"], ",\n"),
    paste0(" version ", pkg.info["Version"], " (built on ", pkg.info["Date"], ").\n"))
}
