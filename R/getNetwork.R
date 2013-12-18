#' Get a particular network
#'
#' Returns a network object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a network
getNetwork <- function(api, id) mangalGet(api, 'network', id)