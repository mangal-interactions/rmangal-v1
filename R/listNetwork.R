#' List the networks currently in the DB
#'
#' Returns a list of networks
#'
#' @param api a \code{\link{mangalapi}} object
listNetwork <- function(api) mangalList(api, 'network')