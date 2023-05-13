#····································································
#   np.svar.npf.R: Estimation of the (intra-curve) semivariogram
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································

#····································································
# svar.bin.npf ----
#····································································
#' Empirical (intra-curve) variogram estimate
#'
#' Creates a \code{svar.bin} (binned semivar. + grid parameters) object with
#' the binned intra-curves semivariances (i.e. computes a binned sample variogram).
#' @rdname svar.bin.npf
#' @aliases svar.bin.npf
# @inheritParams npsp::svar.bin
#' @param  x 	object used to select a method.
#' @param  maxlag maximum lag. Defaults to `(nlags - 1) * minlag`.
#' @param  nlags number of lags. Defaults to `round(0.55 * x$grid$n)`.
#' @param  minlag minimun lag. Defaults to that of the discretization grid.
#' @param  ... 	further arguments passed to [`npsp::svariso`].
#' @param estimator character, estimator name (e.g. `"classical"`).
#' See [`npsp::svar.bin`].
#' @return Returns an object of S3 class [`npsp::svar.bin`].
#' @details
#' Only one of the arguments \code{maxlag} or \code{nlags} must be specified.
#'
#' `svar.bin.npf.data` assumes a stationary functional process (the functional mean and
#' functional variance are constants).
#' @seealso [`npsp::svar.bin`]
#' @method svar.bin npf.data
#' @export
#····································································
svar.bin.npf.data <- function(x, maxlag = NULL, nlags = round(0.55 * x$grid$n),
                              minlag = x$grid$lag, estimator = c("classical", "modulus"), ...) {
#····································································
  if(!is.fd1d(x))
    stop("This method is currently only implemented for onedimensional functional data.")
  if(!is.null(maxlag)) {
    if (!missing(nlags) ) {
      warning("argument 'maxlag' ignored (set to default value)")
      maxlag <- NULL
    } else nlags <- round(maxlag/minlag) + 1
  }
  if(is.null(maxlag)) maxlag <- (nlags - 1) * minlag   # 55% of largest lag by default
  coord <- coords(x)
  result <- apply(x$y, 2,
                  function(y) svariso(coord, y, maxlag = maxlag, nlags = nlags, estimator = estimator, ...))
  bin.sv <- sapply(result, function(x) x$biny)
  bin.w <- sapply(result, function(x) x$binw)
  result <- result[[1]]
  result$biny <- rowMeans(bin.sv)
  result$binw <- rowSums(bin.w)
  # PENDENTE: Revisar uso result$data
  result$data <- x
  result$data$x <- coord
  result$data$med <- mean(result$biny)
  return(result)
}


#' @rdname svar.bin.npf
#' @param var (optional) a vector or an object of class [`npf.var`] with the estimated (or
#' theoretical) variances.
#' @method svar.bin npf.locpol
#' @export
#····································································
svar.bin.npf.locpol <- function(x, var, maxlag = NULL, nlags, minlag,
                                estimator = c("classical", "modulus"), ...) {
# svar.bin.npf.locpol <- function(x, maxlag = NULL, nlags = round(0.55 * x$grid$n),
#                                   minlag = x$grid$lag, estimator = c("classical", "modulus"), ...) {
#····································································
  y <- x$data
  y$y <- x$data$y - x$est
  if(!missing(var)){
    if(inherits(var, "npf.var")) var <- var$est
    y$y <- y$y/sqrt(var)
  }
  result <- svar.bin.npf.data(y, maxlag = maxlag, nlags = nlags, estimator = estimator, ...)
  return(result)
}


#····································································
# np.svar.npf ----
#····································································
#' Local polynomial estimation of the (intra-curve) semivariogram
#'
#' Creates a \code{svar.bin} (binned semivar. + grid parameters) object with
#' the binned intra-curves semivariances (i.e. computes a binned sample variogram).
#' @rdname np.svar.npf
#' @inheritParams svar.bin.npf.data
#' @inheritParams locpol.npf.data
#' @return Returns an object of S3 class [`npsp::svar.bin`].
#' @details
#' Only one of the arguments \code{maxlag} or \code{nlags} must be specified.
#' @seealso [`npsp::svar.bin`]
#' @method np.svar npf.data
#' @export
#····································································
np.svar.npf.data <- function(x, h = NULL, maxlag = NULL, nlags = round(0.55 * x$grid$n),
                             minlag = x$grid$lag, degree = 1,
                             drv = FALSE, hat.bin = TRUE, ncv = 0, ...) {
  #····································································
  bin.svar <- svar.bin(x, maxlag = maxlag, nlags = nlags, minlag = minlag, ...)
  return(npsp::np.svar(bin.svar, h = h, degree = degree, drv = drv, hat.bin = hat.bin,
                 ncv = ncv))
}


#····································································
#' @rdname np.svar.npf
#' @method np.svar npf.locpol
#' @export
#····································································
np.svar.npf.locpol <- function(x, var, h = NULL, maxlag = NULL, nlags = round(0.55 * x$grid$n),
                             minlag = x$grid$lag, degree = 1,
                             drv = FALSE, hat.bin = TRUE, ncv = 0, ...) {
#····································································
  bin.svar <- svar.bin(x, var, maxlag = maxlag, nlags = nlags, minlag = minlag, ...)
  return(npsp::np.svar(bin.svar, h = h, degree = degree, drv = drv, hat.bin = hat.bin,
                 ncv = ncv))
}

