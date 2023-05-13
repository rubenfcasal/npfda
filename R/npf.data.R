#' Functional data (S3 class "npf.data")
#'
#' Defines a multidimensional functional data set.
#' Constructor function of the \code{npf.data}-\code{\link{class}}.
#' @aliases npf.data-class
#' @param y numeric matrix or array; the last dimension corresponds to curves and
#' the first ones to discretization points.
#' @param min vector; minimum values of the discretization points (coordinates).
#' @param max vector; maximum values of the discretization points (optional).
#' @param lag vector; lag in each dimension (optional).
#' @param ylabel character; name used to label the data.
#' @param ynames character vector; names used to label the curves.
#' @param dimnames character vector; names used to label the dimensions.
#' @details
#' Only one of the arguments \code{max} or \code{lag} must be specified.
#' @return Returns an object of \code{\link{class}} \code{npf.data}
#' (extending \code{npsp::data.grid}), a list with
#' the arguments `y`, `ylabel` and `ynames` as components, and two additional
#' components:
#'
#' * `ny` number of curves (sample size).
#' * `grid` a [npsp::grid.par]-class object that defines the
#' discretization points.
#' @seealso [`npsp::data.grid`].
#' @export
#' @examples
#' fd.ozone <- npf.data(ozone, dimnames = "day")
#' plot(fd.ozone)
#····································································
npf.data <- function(y, min, max, lag, ylabel, ynames, dimnames) {
#····································································
  dim.y <- dim(y)
  if(is.null(dim.y) || length(dim.y) < 2 || !is.numeric(y))
    stop("argument 'y' must be a numeric matrix or array.")
  if (missing(ylabel))
      ylabel <- deparse(substitute(y))
  ncur <- dim.y[length(dim.y)]
  if (missing(ynames)){
    ynames <- base::dimnames(y)[[length(dim.y)]]
    if (is.null(ynames)) ynames <- seq_len(ncur) else  base::dimnames(y) <- NULL
  }
  nd <- length(dim.y) - 1
  n <- dim.y[seq_len(nd)] # nx
  # Dejamos a grid.par que chequee sus parametros
  if (missing(min)) min <- rep(1, nd)
  if (missing(max))
    max <- if (missing(lag)) n else min + (n - 1) * lag
  if (missing(dimnames))
    dimnames <- if (nd == 1) "t" else NULL
  grid <- npsp::grid.par(n = n, min = min, max = max, dimnames = dimnames)
  result <- list(y = y, ny = ncur, ylabel = ylabel, ynames = ynames, grid = grid)
  oldClass(result) <- c( "npf.data", "data.grid")
  return(result)
}


# #' @method coords npf.data
# #' @param  masked IGNORED (included for compatibility with `coords.data.grid`).
# #' @export
# coords.npf.data <- function(x, masked = FALSE, ...) {
# #····································································
#     return(drop(coords(x$grid)))
# }

#' @rdname npf.data
#' @method coords npf.data
#' @param x funcional data ([npf.data]-[class] object).
#' @param  ... 	further arguments passed to or from other methods.
#' @export
coords.npf.data <- function(x, ...) {
#····································································
    return(drop(coords(x$grid)))
}




#' Plot functional data
#'
#' Plots a functional data object.
#'
#' @method plot npf.data
#' @param x funcional data ([npf.data]-[class] object).
#' @param y numerical vector containing the values used for coloring the curves.
#' @param legend logical; if `TRUE` (default), the plotting region is splitted into two parts,
#' drawing the main plot in one and the legend with the color scale in the other.
#' If `FALSE` only the (coloured) main plot is drawn and the arguments related
#' to the legend are ignored ([npsp::splot()] is not called).
#' @param bigplot plot coordinates for main plot. If not passed, and `legend`
#' is `TRUE`, these will be determined within the function.
#' @param smallplot plot coordinates for legend strip. If not passed, and `legend`
#' is `TRUE`, these will be determined within the function.
#' @param add logical; if `TRUE` the plot is just added
#' to the existing plot.
#' @param reset logical; if `FALSE` the plotting region
#' (`par("plt")`) will not be reset to make it possible to add more features
#' to the plot (e.g. using functions such as points or lines). If `TRUE` (default)
#' the plot parameters will be reset to the values before entering the function.
#' @param lty vector of line types.
#' @param xlab label for the x axis, defaults to `x$grid$dimnames`.
#' @param ylab label for the y axis, defaults to `x$ylabel`.
#' @param ... additional graphical parameters (to be passed to [matplot()]
#' or [matlines()].
#' NOTE: graphical arguments passed here will only have impact on the main plot.
#' To change the graphical defaults for the legend use the [par()]
#' function beforehand (e.g. `par(cex.lab = 2)` to increase colorbar labels).
#' @inheritParams npsp::splot
#' @keywords hplot
#' @examples
#' fd <- npf.data(ozone, dimnames = "day")
#' plot(fd)
#' plot(fd, y = as.numeric(fd$ynames))
#' @seealso [npsp::splot()], [matplot()], [matlines()]
#' @export
#····································································
plot.npf.data <- function(x, y = seq_len(x$ny), slim = range(y, finite = TRUE),
    col = rainbow(128, end = 0.75), breaks = NULL, legend = TRUE, horizontal = FALSE,
    legend.shrink = 1.0, legend.width = 1.2, legend.mar = ifelse(horizontal, 3.1, 5.1),
    legend.lab = NULL, bigplot = NULL, smallplot = NULL, lab.breaks = NULL, axis.args = NULL,
    legend.args = NULL, add = FALSE, reset = TRUE,
    lty = 1, xlab = x$grid$dimnames, ylab = x$ylabel, ...) {
#····································································
  if(!is.fd1d(x))
    stop("This method is currently only implemented for onedimensional functional data.")
  if (legend)
      # image in splot checks breaks and other parameters...
      res <- npsp::splot(slim = slim, col = col, breaks = breaks, horizontal = horizontal,
          legend.shrink = legend.shrink, legend.width = legend.width,
          legend.mar = legend.mar, legend.lab = legend.lab,
          bigplot = bigplot, smallplot = smallplot, lab.breaks = lab.breaks,
          axis.args = axis.args, legend.args = legend.args, add = add)
  else {
      if (missing(bigplot)) {
        old.par <- list(plt = par("plt")) # par("plt", "mfg", "xpd", "pty")
        bigplot <- old.par$plt
      } else
        old.par <- par(plt = bigplot)
      # par(xpd = FALSE)
      res <- list(bigplot = bigplot, smallplot = NA, old.par = old.par)
  }
  if (add & !reset) {
      # Creo que realmente no haria falta...
      warning("'reset' argument ignored when 'add = TRUE'")
      reset <- TRUE
  }
  if (reset) on.exit(par(res$old.par))
  if (is.null(breaks)) {
      # Compute breaks (in 'cut.default' style...)
      ds <- diff(slim)
      if (ds == 0) ds <- abs(slim[1L])
      breaks <- seq.int(slim[1L] - ds/1000, slim[2L] + ds/1000, length.out = length(col) + 1)
      # Only if !missing(slim) else breaks <- length(col) + 1?
  }
  icol <- cut(y, breaks, labels = FALSE, include.lowest = TRUE, right = FALSE) # Use .bincode instead of cut?
  if (!add) {
    matplot(coords(x), x$y, type = "l", lty = lty, col = col[icol],
        xlab = xlab, ylab = ylab, ...)
  } else
    matlines(coords(x), x$y, lty = lty, col = col[icol], ...)
  # if (reset) par(res$old.par)
  return(invisible(res))
#····································································
}   # plot.npf.data




