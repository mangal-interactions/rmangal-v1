#' Get a particular dataset
#'
#' Returns a dataset object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a dataset
getDataset <- function(api, id) mangalGet(api, 'dataset', id)