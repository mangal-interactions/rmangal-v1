#' @title List the taxa currently in the DB
#'
#' @description Returns a list of taxa
#'
#' @param api a \code{\link{mangalapi}} object
listTaxa <- function(api) mangalList(api, 'taxa')

#' @title Get a particular taxa
#'
#' @description Returns a taxa object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a taxa
getTaxa <- function(api, id) mangalGet(api, 'taxa', id)