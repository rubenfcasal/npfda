#····································································
#   f.stats.R:
#   Functions for statistical calculations and random number generation
#   related to functional data.
#····································································
#   Utilities to interact with the rainbow package
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································


#····································································
# f.mean ----
#····································································
#' Sample functional mean and variance
#'
#' `f.mean` computes the sample functional mean and `f.var` the sample
#' functional variance by default.
#' @aliases f.stats
#' @param x a functional data object.
#' @param ... further arguments passed to or from other methods.
#' @return Return a vector with the corresponding estimates at the
#' discretization points.
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' # Plot data + sample mean -+ sample std. dev.
#' plot(fd, col = "lightgray", legend = FALSE)
#' x <- coords(fd)
#' y <- f.mean(fd)
#' lines(x, y)
#' matlines(x, y + sqrt(f.var(fd, mean = y)) %o% c(-1, 1), col = 1, lty = 2)
#' @export
#····································································
f.mean <- function(x, ...){
#····································································
  UseMethod("f.mean")
}


#' @rdname f.mean
#' @method f.mean npf.data
#' @param na.rm logical; indicating whether `NA` values should be ignored.
#' @export
#····································································
f.mean.npf.data <- function(x, na.rm = TRUE, ...) {
  return(rowMeans(x$y, na.rm = na.rm))
}



#····································································
# f.var ----
#····································································
#' @rdname f.mean
#' @export
#····································································
f.var <- function(x, ...){
#····································································
  UseMethod("f.var")
}


#' @rdname f.mean
#' @method f.var npf.data
#' @param mean numeric; mean estimates at the discretization points.
#' @details
#' `f.var.npf.data()` computes the sample functional variance by default,
#' using the denominator \eqn{n - 1}
#' (alternatively, setting `mean = 0` may be appropriate for residuals or
#' `mean = predict(fit)`, for computing a residual functional variance).
#' @export
#····································································
f.var.npf.data <- function(x, mean = f.mean(x, na.rm = na.rm), na.rm = TRUE, ...) {
  r2 <- (x$y - mean)^2
  return(rowSums(r2, na.rm = na.rm)/(rowSums(!is.na(r2)) - 1))
}
