#' List the datasets currently in the DB
#'
#' Returns a list of datasets
#'
#' @param api a \code{\link{mangalapi}} object
listDataset <- function(api) mangalList(api, 'dataset')
