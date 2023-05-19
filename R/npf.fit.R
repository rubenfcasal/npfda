#····································································
#   npf.fit.R: Fit a nonparametric functional model
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································


#····································································
#····································································
#' Nonparametric functional model
#'
#' Defines a nonparametric functional model.
#' Constructor function of the S3 class `npf.model`.
#' @aliases npf.model-class
#' @param  lp local polynomial estimate of the functional trend (object of class
#' [`npf.locpol`]).
#' @param  lp local polynomial estimate of the functional variance (object of
#' class [`npf.var`]).
#' @param  svm fitted variogram model (object of class [`npsp::fitsvar`]).
#' @return Returns an object of class `npf.model` (extending [`npf.locpol`]),
#' the `lp` argument with the others as additional components.
#' @seealso [`npf.fit`], [`npf.locpol`], [`npf.var`], [`npsp::fitsvar.sb.iso`].
#' @export
npf.model <- function(lp, var, svm) {
#····································································
  result <- lp
  result$var <- var
  result$svm <- svm
  # result$cor <- varcov(svm, lp$data$x)
  oldClass(result) <- unique(c("npf.model", oldClass(result)))
  return(result)
}


#' @rdname npf.model
#' @inheritParams residuals.npf.locpol
#' @method residuals npf.model
#' @export
residuals.npf.model <- function(object, as.npf.data = TRUE, ...) {
#····································································
  if(as.npf.data) return(object$svm$esv$data)
  return(object$svm$esv$data$y)
}


#' @rdname npf.model
#' @param x a nonparametric functional model object.
#' Typically an output of [`npf.fit`].
#' @param y	ignored argument.
#' @param main.trend title for the trend plot.
#' @param main.svar title for the semivariogram plot.
#' @param ... additional graphical parameters (passed to [`plot.npf.locpol`]).
#' @method plot npf.model
#' @export
plot.npf.model <- function(x, y = NULL, main.trend = "Functional trend estimate",
                        main.svar = "Semivariogram estimate", ...) {
#····································································
  old.par <- par(mfrow = c(1,2)) #, omd = c(0.01, 0.9, 0.05, 0.95))
  on.exit(par(old.par))
  plot.npf.locpol(x, y = x$var, main = main.trend, ...)
  plot(x$svm, main = main.svar)
  # par(old.par)
}



#····································································
# npf.fit(x, ...) ----
#····································································
#' Fit a nonparametric functional model
#'
#' Fits a nonparametric functional model
#' (jointly estimates the trend, the variance and the variogram) by calling
#' [`locpol.npf.bin`], [`np.var.npf.bin.res2`], [`np.svar.npf.locpol`] and
#' [`npsp::fitsvar.sb.iso`] iteratively.
#' At each iteration, the trend and variance estimation bandwidths are updated
#' by calling [`h.cv`](h.cv.npf).
#' @param x a (data) object used to select a method.
#' @param ... further arguments passed to [`h.cv.npf.bin`]
#' (trend bandwidth selection parameters).
#' @return Returns an object of class `npf.model`
#' (extending [`npf.locpol`]), the `lp` argument with the others as additional
#' components, and an additional component `convergence` with the number of iterations and
#' the relative differences in trend and variance bandwidth selections.
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' fit <- npf.fit(fd, maxlag = 100, var.h = 30, verbose = TRUE)
#' plot(fit)
#' @seealso [`npf.model`], [`locpol.npf.bin`], [`np.var.npf.bin.res2`],
#' [`np.svar.npf.locpol`], [`h.cv.npf.bin`], [`h.cv.npf.bin.res2`], [`npsp::fitsvar.sb.iso`].
#' @export
#····································································
npf.fit <- function(x, ...) {
  UseMethod("npf.fit")
}


#' @rdname npf.fit
#' @param iter maximum number of iterations.
#' @param tol relative convergence tolerance (trend and variance bandwidths).
#' @param  h initial bandwidth for trend estimation
#' (final bandwidth if \code{iter = 0}).
#' @param var.h initial bandwidth for variance estimation
#' (final bandwidth if \code{iter = 0}).
#' @param svar.h bandwidth for variogram estimation.
#' @param maxlag,nlags maximum lag and number of lags in variogram estimation
#' (only one must be specified; see [`np.svar.npf.locpol`]).
#' @param dk dimension of the Shapiro-Botha variogram model (see [`npsp::fitsvar.sb.iso`]).
#' @param verbose logical; if \code{TRUE}, the errors (the relative differences
#' in trend and variance bandwidth selections) at each iteration are printed.
#' @method npf.fit default
#' @export
#····································································
npf.fit.default <- function(x, iter = 2, tol = 0.1, h = NULL, var.h = NULL,
            svar.h = NULL, maxlag = NULL, nlags, dk = 0, verbose = FALSE, ...) {
#····································································
  if(!inherits(x, "npf.data")) stop("'x' is not a functional data object")
  # Binning of functional data
  bin <- npf.binning(x) # binning
  if(is.null(h)) h <- 2*h.cv(bin, ...)$h
  # Linear Local trend estimates
  lp <- locpol(bin, h = h)
  # Binning of squared residuals
  bin.res2 <- npf.bin.res2(lp)
  if(is.null(var.h)) var.h <- 2*h.cv(bin.res2)$h
  # Linear Local variance estimate
  lp.var <- np.var(bin.res2, h = var.h)
  # Binned intra-curves semivariances
  bin.svar <- svar.bin(lp, lp.var, maxlag = maxlag, nlags = nlags)
  if(is.null(svar.h))  svar.h <- 1.5*h.cv(bin.svar)$h
  # Linear local polynomial (intra-curve) semivariogram estimate
  svar.np <- np.svar(bin.svar, h = svar.h)
  # Fitted variogram model
  svm <- fitsvar.sb.iso(svar.np, dk = dk)
  # Return
  if(verbose){
    cat("\nIteration:", 0, "\n")
    cat("Trend bandwidth:", h, "\n")
    cat("Variance bandwidth:", var.h, "\n")
    cat("Semivariogram bandwidth:", svar.h, "\n")
  }
  result <- npf.model(lp, lp.var, svm)
  if(iter > 0)
    result <- npf.fit.npf.model(result, iter = iter, tol = tol, svar.h = svar.h,
                                dk = dk, verbose = verbose, ...)
  return(result)
}


#' @rdname npf.fit
#' @method npf.fit npf.model
#' @export
#····································································
npf.fit.npf.model <- function(x, iter = 1, tol = 0.1, svar.h = x$svm$esv$locpol$h,
                              dk = x$svm$par$dk, verbose = FALSE, ...) {
#····································································
  lp <- x
  lp.var <- x$var
  svm <- x$svm
  nlags <- svm$esv$grid$n
  for (i in 1:iter) {
    # Estimated correlation matrix
    cor.est <- varcov(svm, lp$data$x)
    # Trend estimation
    # h <- if(hasArg("h.start")) h.cv(lp, lp.var, cor = cor.est, ...)$h else
    #   h.cv(lp, lp.var, cor = cor.est, h.start = as.vector(lp$locpol$h), ...)$h
    h <-  h.cv(lp, lp.var, cor = cor.est, h.start = as.vector(lp$locpol$h), ...)$h
    error.trend.h <- abs(lp$locpol$h/h - 1)
    lp <- locpol(lp, h = h)
    # Variance estimation
    var.h <- h.cv(lp.var, lp.var, cor = cor.est,
                  h.start = as.vector(lp.var$locpol$h))$h
    error.var.h <- abs(lp.var$locpol$h/var.h - 1)
    lp.var <- np.var(lp.var, h = var.h)
    # Semivariogram estimation
    svar.np <- np.svar(lp, lp.var, h = svar.h, nlags = nlags)
    svm <- fitsvar.sb.iso(svar.np, dk = dk)
    if(verbose){
      cat("\nIteration:", i, "\n")
      cat("Trend bandwidth:", h, ", Error:", error.trend.h, "\n")
      cat("Variance bandwidth:", var.h, ", Error:", error.var.h, "\n")
    }
    # Check convergence
    if (max(error.trend.h, error.var.h) < tol) break
  }
  result <- npf.model(lp, lp.var, svm)
  result$convergence$iter <- iter
  result$convergence$error.trend.h <- error.trend.h
  result$convergence$error.var.h <- error.var.h
  return(result)
}



