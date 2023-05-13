#····································································
#   locpol.npf.R: Local polynomial estimation of the functional trend
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································


#····································································
# npf.binning ----
#····································································
#' Binning of functional data
#'
#' Aggregates the functional data into the regular grid of discretization points,
#' i.e. computes the functional sample mean and the sample size.
#' @aliases npf.bin-class npf.bin
#' @param x funcional data ([`npf.data`]-[`class`] object).
#' @return Returns an S3 object of class `npf.bin` extending
#' [`npsp::bin.data`] (bin data + grid par.).
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' bin <- npf.binning(fd)
#' plot(fd, col = "lightgray", legend = FALSE)
#' with(bin, lines(data$x, biny))
#' @seealso [`npsp::bin.data`], [`locpol.npf.bin`]
#' @export
#····································································
# PENDENTE:
# Comprobar donde se emplea $data
# - locpol.bin.data: $data$med
# - residuals.locpol.bin
# - predict.locpol.bin
# Hacer data <- x sin grid y añadir componentes
#····································································
npf.binning <- function(x){
  if(!is.fd1d(x))
    stop("This method is currently only implemented for onedimensional functional data.")
  result <- with(x,
            npsp::data.grid(biny = rowMeans(y, na.rm = TRUE), binw = rowSums(!is.na(y)),
            grid = grid ))
  result$data <- x
  result$data$x <- coords(x)
  result$data$med <- mean(result$biny)
  # result$data <- list(x = rep(coords(x), x$ny), y = as.vector(x$y), med = mean(result$biny))
  oldClass(result) <- c("npf.bin", "bin.data", "bin.den", "data.grid")
  return(result)
}


#····································································
# locpol.npf ----
#····································································
#' Local polynomial trend estimation
#'
#' Estimates the functional mean (and its first derivatives)
#' using local polynomial kernel smoothing.
#' @aliases npf.locpol-class npf.locpol
#' @param  x 	an object (of class [`npf.data`] or [`npf.bin`]) used to select a method.
#' @param  ... 	further arguments passed to or from other methods.
#' @inheritParams npsp::locpol.bin
#' @return `locpol.npf.data` and  `locpol.npf.bin` return an S3 object of class
#' `npf.locpol` extending [`npsp::locpol.bin`] (locpol + bin data + grid par.).
#' @seealso [`npsp::locpol.bin`], [`npf.bin`]
#' @method locpol npf.data
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' # Linear Local trend estimate
#' lp <- locpol(fd, h = 35)
#' # Plot
#' plot(fd, col = "lightgray", legend = FALSE)
#' lines(lp$data$x, lp$biny, lty = 2) # x = coords(fd)
#' lines(lp$data$x, lp$est)
#' # Bandwidth selection
#' # (assuming independence and homoscedasticity)
#' bin <- npf.binning(fd) # binning
#' trend.h <- h.cv(bin)$h
#' trend.h
#' # the selected bandwidth undersmoothes the data...
#' trend.h <- 4*trend.h
#' # Linear Local trend estimate
#' lp <- locpol(bin, h = trend.h, hat.bin = TRUE)
#' # Plot
#' plot(fd, col = "lightgray", legend = FALSE)
#' lines(lp$data$x, lp$biny, lty = 2)
#' lines(lp$data$x, lp$est)
#' @export
#····································································
locpol.npf.data <- function(x, h = NULL, degree = 1 + as.numeric(drv), drv = FALSE,
                                        hat.bin = FALSE, ncv = 0, ...) {
  bin <- npf.binning(x)
  return(locpol.npf.bin(bin, h = h, degree = degree, drv = drv,
                             hat.bin = hat.bin, ncv = ncv, ...))
}


#' @rdname locpol.npf.data
#' @method locpol npf.bin
#' @export
#····································································
locpol.npf.bin <- function(x, h = NULL, degree = 1 + as.numeric(drv), drv = FALSE,
                                        hat.bin = FALSE, ncv = 0, ...) {
  result <- locpol(as.bin.data(x), h = h, degree = degree, drv = drv,
                             hat.bin = hat.bin, ncv = ncv, ...)
  result$data <- x$data
  oldClass(result) <- unique(c("npf.locpol", "npf.bin", oldClass(result)))
  return(result)
}


#' @rdname locpol.npf.data
#' @method predict npf.locpol
#' @param object object used to select a method.
#' @param newx vector with the (irregular) points to predict (interpolate).
#' @return If `newx == NULL`, `predict.npf.locpol` returns the trend estimates
#' corresponding to the discretization points
#' (otherwise `npsp::interp.data.grid` is called).
#' @export
predict.npf.locpol <- function(object, newx = NULL, ...) {
  #····································································
  if (!is.null(newx)) {
    return(npsp::interp(object, data.ind = 'est', newx = newx, ...))
  } else return(object$est)
}


