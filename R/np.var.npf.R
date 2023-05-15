#····································································
#   np.svar.npf.R:  Local polynomial estimation of the functional variance
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································


#' Binning of squared residuals of a local polynomial fit
#'
#' Aggregates the squared residuals of a local polynomial kernel smoothing
#' into the regular grid of discretization points,
#' i.e. computes the functional residual variance (using \eqn{n} as
#' denominator) and the sample size.
#' @aliases npf.bin.res2-class npf.bin.res2
#' @param x local polynomial fit ([`npf.locpol`]-[`class`] object).
#' @return Returns an S3 object of class `npf.bin.res2` extending
#' [`npsp::bin.data`] (bin data + grid par.).
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' # Linear Local trend estimate
#' lp <- locpol(fd, h = 35)
#' # Bandwidth selection for variance estimation
#' bin.res2 <- npf.bin.res2(lp)
#' h.cv(bin.res2)
#' @seealso [`npf.binning`], [`npf.var`]
#' @export
#····································································
npf.bin.res2 <- function(x){
  if (!inherits(x, "npf.locpol"))
    stop("function only works for objects of class (or extending) 'npf.locpol'")
  r2 <- (x$data$y - x$est)^2
  result <- npsp::data.grid(biny = rowMeans(r2, na.rm = TRUE), binw = rowSums(!is.na(r2)),
            grid = x$grid)
  result$data <- x$data
  result$data$y <- r2
  result$data$ylabel  <- paste0("Squared residuals of '", x$data$ylabel, "'")
  result$data$med = mean(result$biny)
  oldClass(result) <- c("npf.bin.res2", "bin.data", "bin.den", "data.grid")
  return(result)
}



#····································································
# np.var.npf ----
#····································································
#' Local polynomial variance estimation
#'
#' Estimates the functional variance (and its first derivatives)
#' using local polynomial kernel smoothing.
#' @aliases npf.var-class npf.var
#' @param  x 	an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @inheritParams npsp::locpol.bin
#' @return Returns an S3 object of class `npf.var` extending [`npsp::locpol.bin`]
#' (locpol + bin data + grid par.).
#' @seealso [`npf.bin.res2`]
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' # Linear Local trend estimate
#' lp <- locpol(fd, h = 35)
#' # Linear Local variance estimate
#' lp.var <- np.var(lp, h = 50)
#' # Plot data + estimated trend -+ estimated std. dev.
#' plot(lp$data, col = "lightgray", legend = FALSE)
#' x <- lp$data$x
#' y <- lp$est
#' lines(x, y)
#' matlines(x, y + sqrt(lp.var$est) %o% c(-1, 1), col = 1, lty = 2)
#' # Bandwidth selection (assuming independence)
#' bin.res2 <- npf.bin.res2(lp)
#' var.h <- h.cv(bin.res2)$h
#' # the selected bandwidth undersmoothes the squared residuals...
#' var.h <- 7*var.h
#' # Linear Local variance estimate
#' lp.var <- np.var(lp, h = var.h)
#' # Plot data + estimated trend -+ estimated std. dev.
#' plot(lp$data, col = "lightgray", legend = FALSE)
#' x <- lp$data$x
#' y <- lp$est
#' lines(x, y)
#' matlines(x, y + sqrt(lp.var$est) %o% c(-1, 1), col = 1, lty = 2)
#' @export
#····································································
np.var <- function(x, ...)  {
#····································································
  UseMethod("np.var")
}



# Preparar método al estilo de  f.var.npf.data?
# np.var.npf.data <- function(x, mean = rowMeans(x$y, na.rm = TRUE), h = NULL,
#         degree = 1 + as.numeric(drv), drv = FALSE, hat.bin = FALSE, ncv = 0, ...) {
#   r2 <- x
#   r2$y <- (r2.y - mean)^2
#   ...
# }



#' @rdname np.var
#' @method np.var npf.locpol
#' @export
#····································································
np.var.npf.locpol <- function(x, h = NULL, degree = 1 + as.numeric(drv), drv = FALSE,
                               hat.bin = FALSE, ncv = 0, ...) {
  bin <- npf.bin.res2(x)
  return(np.var.npf.bin.res2(bin, h = h, degree = degree, drv = drv,
                             hat.bin = hat.bin, ncv = ncv, ...))
}


#' @rdname np.var
#' @method np.var npf.bin.res2
#' @export
#····································································
np.var.npf.bin.res2 <- function(x, h = NULL, degree = 1 + as.numeric(drv), drv = FALSE,
                                        hat.bin = FALSE, ncv = 0, ...) {
  result <- locpol(as.bin.data(x), h = h, degree = degree, drv = drv,
                             hat.bin = hat.bin, ncv = ncv, ...)
  result$data <- x$data
  oldClass(result) <- unique(c("npf.var", "npf.bin.res2", oldClass(result)))
  return(result)
}


#' @rdname np.var
#' @method predict npf.var
#' @param object object used to select a method.
#' @param newx vector with the (irregular) points to predict (interpolate).
#' @return If `newx == NULL`, `predict.npf.var` returns the variance estimates
#' corresponding to the discretization points
#' (otherwise `npsp::interp.data.grid` is called).
#' @export
predict.npf.var <- function(object, newx = NULL, ...) {
  #····································································
  if (!is.null(newx)) {
    return(npsp::interp(object, data.ind = 'est', newx = newx, ...))
  } else return(object$est)
}







