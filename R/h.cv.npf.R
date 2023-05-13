#····································································
#   h.cv.npf.R:  Bandwidth selection for local polynomial estimation
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································

#' Cross-validation methods for bandwidth selection
#'
#' Selects the bandwidth of a local polynomial kernel (regression or
#' variance) estimator using (standard or modified) CV, GCV or MASE criteria.
#' See [`npsp::h.cv`] for details.
#' @rdname h.cv.npf
#' @aliases h.cv.npf
#' @param bin object used to select a method (binned data).
#' @param var (optional) a vector or an object of class [`npf.var`] with the estimated (or
#' theoretical) variances.
#' @param cor (optional) intra-curve correlation matrix or semivariogram model
#' (\code{\link{svarmod}}-class) of the data. Defaults to the identity matrix.
#' @param ... further arguments passed to [`npsp::h.cv`].
#' @seealso [`npsp::h.cv`], [`npf.binning`], [`npf.bin.res2`]
#' @method h.cv npf.bin
#' @export
#····································································
h.cv.npf.bin <- function(bin, var, cor, ...) {
#····································································
  if(!missing(var)) {
    if(inherits(var, "npf.var")) var <- var$est
  } else{
    var <- 1
  }
  if(!missing(cor)) {
    p <- (d <- dim(cor))[1L]
    if (!is.numeric(cor) || length(d) != 2L || p != d[2L])   # check if not square matrix...
      if (!inherits(cor, "svarmod")) {  # semivariogram model
        stop("'cor' must be a square matrix or a semivariogram model ('svarmod' class).")
      } else {
        cor <- varcov(cor, coords = bin$data$x)
      }
  } else
    cor <- diag(bin$grid$n)
  # Covariance of binned data
  sd.bin <- sqrt(var/bin$binw)
  cov.bin <- tcrossprod(sd.bin) * cor
  return(npsp::h.cv(as.bin.data(bin), cov.bin = cov.bin, ...))
}



#' @rdname h.cv.npf
#' @method h.cv npf.bin.res2
#' @export
#····································································
h.cv.npf.bin.res2 <- function(bin, var, cor, ...) {
#····································································
  if(!missing(var)) {
    if(inherits(var, "npf.var")) var <- var$est
  } else{
    var <- 1
  }
  if(!missing(cor)) {
    p <- (d <- dim(cor))[1L]
    if (!is.numeric(cor) || length(d) != 2L || p != d[2L])   # check if not square matrix...
      if (!inherits(cor, "svarmod")) {  # semivariogram model
        stop("'cor' must be a square matrix or a semivariogram model ('svarmod' class).")
      } else {
        cor <- varcov(cor, coords = bin$data$x)
      }
  } else
    cor <- diag(bin$grid$n)
  # Covariance of binned squared residuals
  sd.bin.r2 <- var/sqrt(bin$binw) # *sqrt(2)
  cov.bin.r2 <- 2 * tcrossprod(sd.bin.r2) * cor^2
  return(npsp::h.cv(as.bin.data(bin), cov.bin = cov.bin.r2, ...))
}





