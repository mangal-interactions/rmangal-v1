#' @title List the interactions currently in the DB
#'
#' @description Returns a list of interactions
#'
#' @param api a \code{\link{mangalapi}} object
listInteraction <- function(api) mangalList(api, 'interaction')

#' @title Get a particular interaction
#'
#' @description Returns a interaction object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a interaction
getInteraction <- function(api, id) mangalGet(api, 'interaction', id)