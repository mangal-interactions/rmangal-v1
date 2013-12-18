#' @title List the datasets currently in the DB
#'
#' @description Returns a list of datasets
#'
#' @param api a \code{\link{mangalapi}} object
listDataset <- function(api) mangalList(api, 'dataset')

#' @title Get a particular dataset
#'
#' @description Returns a dataset object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a dataset
getDataset <- function(api, id) mangalGet(api, 'dataset', id)