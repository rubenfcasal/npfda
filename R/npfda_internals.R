#' @name npfda-internals
#' @aliases is.fd1d
#' @title npfda internal and secondary functions
#' @description Listed below are supporting functions for the major methods in npfda.
#' @keywords internal
is.fd1d <- function(object){
  return(inherits(object, "npf.data") && object$grid$nd == 1)
}

