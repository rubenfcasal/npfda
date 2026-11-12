#····································································
#   Utilities to interact with the fda.usc package
#
#   Authors: Manuel Oviedo, Ruben Fernandez-Casal
#   Created:  2026/11
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································

#····································································
# as.fdata methods ---
#····································································

#' Converts functional data to a fdata object
#'
#' Converts a functional data object to a \link[fda.usc]{fdata} object.
#'
#' @param obj a functional data object.
#' @param ... further arguments passed to or from other methods.
#' @return Returns a [fda.usc::fdata]-class object.
# @seealso [fda.usc::fdata()]
#' @export
#····································································
as.fdata <- function(obj, ...)  {
  UseMethod("as.fdata")
}


#' @rdname as.fdata
#' @method as.fdata npf.data
#' @export
as.fdata.npf.data <- function(obj, ...) {
  # obj <- npf.data(ozone, dimnames = "day")
  if(!is.fd1d(obj))
    stop("This method is currently implemented only for onedimensional functional data.")
  mdata <- t(as.matrix(obj$y))
  rownames(mdata) <- obj$ynames
  rangeval <- with(obj$grid, c(min, max))
  names <- list(
    main = obj$ylabel,
    xlab = if (!is.null(obj$dimnames)) obj$dimnames else "argvals",
    ylab = obj$ylabel
  )
  return(fda.usc::fdata(mdata = mdata, argvals = coords(obj),
                        rangeval = rangeval, names = names))
}


#····································································
# as.npf.data fdata ---
#····································································


#' @rdname npf.data
#' @method as.npf.data fdata
# @param ylabel label for the functional response. By default, uses obj$names$ylab when available.
# @param ynames curve names. By default, uses row names of obj$data.
# @param dimnames name of the functional argument. By default, uses obj$names$xlab when available.
#' @param tol numerical tolerance used to check whether the discretization points are regularly spaced
#' @export
as.npf.data.fdata <- function(obj, ylabel = NULL, ynames = NULL, dimnames = NULL,
                              tol = sqrt(.Machine$double.eps), ...) {
  dat <- as.matrix(obj$data)
  if(!is.numeric(dat) || length(dim(dat))!=2L)
    stop("`obj$data` must be a numeric matrix")
  storage.mode(dat) <- "double"

  tt <- obj$argvals
  if(is.list(tt)) {
    if(length(tt)!=1L)
      stop("Only one-dimensional fdata objects are currently supported")
    tt <- tt[[1L]]
  }
  tt <- as.numeric(tt)
  if(length(tt)!=ncol(dat) || any(!is.finite(tt)))
    stop("`obj$argvals` is incompatible with `obj$data`")
  # Deliberately rejects irregular grids because `npf.data` cannot represent
  # arbitrary irregular `fdata$argvals` without changing the observation locations.
  # PENDIENTE: Opción de interpolar?
  if(length(tt)>1L) {
    dtt <- diff(tt)
    if(any(dtt<=0)) stop("`obj$argvals` must be strictly increasing")
    if(length(dtt)>1L && max(abs(dtt-dtt[1L]))>tol*max(1,abs(dtt[1L])))
      stop("npf.data requires a regular grid; `obj$argvals` is not equally spaced")
  }

  if(is.null(ylabel)) {
    ylabel <- obj$names$ylab
    if(is.null(ylabel) || !length(ylabel)) ylabel <- deparse(substitute(fdobj))
  }
  if(is.null(ynames)) {
    ynames <- rownames(dat)
    if(is.null(ynames)) ynames <- as.character(seq_len(nrow(dat)))
  }
  if(is.null(dimnames)) {
    dimnames <- obj$names$xlab
    if(is.null(dimnames) || !length(dimnames)) dimnames <- "t"
  }

  return(npf.data(y = t(dat), min = tt[1L], max = tt[length(tt)], ylabel = ylabel,
           ynames = ynames, dimnames = dimnames, ...))
}


