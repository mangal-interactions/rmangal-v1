#' @title List the networks currently in the DB
#'
#' @description Returns a list of networks
#'
#' @param api a \code{\link{mangalapi}} object
listNetwork <- function(api) mangalList(api, 'network')

#' @title Get a particular network
#'
#' @description Returns a network object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a network
getNetwork <- function(api, id) mangalGet(api, 'network', id)