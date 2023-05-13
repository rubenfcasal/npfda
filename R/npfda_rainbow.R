#····································································
#   Utilities to interact with the rainbow package
#
#   Author: Ruben Fernandez-Casal
#   Created: May 2023
#
#   NOTE: Press Ctrl + Shift + O to show document outline in RStudio
#····································································


#' Converts functional data to a fds object
#'
#' Converts a functional data object to a \link[rainbow:fds]{fds} object.
#'
#' @param obj a functional data object.
#' @param ... further arguments passed to or from other methods.
#' @return Returns a [rainbow::fds]-class object.
# @seealso [rainbow::fds()]
#' @export
#····································································
as.fds <- function(obj, ...)  {
  UseMethod("as.fds")
}


#' @rdname as.fds
#' @method as.fds npf.data
#' @examples
#' fdata <- npf.data(ozone, dimnames = "day")
#' rainbow::plot.fds(as.fds(fdata))
#' @export
#····································································
as.fds.npf.data <- function(obj, ...) {
  if(!is.fd1d(obj))
    stop("This method is currently only implemented for onedimensional functional data.")
  y <- obj$y
  colnames(y) <- obj$ynames
  return(rainbow::fds(coords(obj), y, obj$grid$dimnames, obj$ylabel))
}
