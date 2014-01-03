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

#' @title Add a new dataset
#' 
#' @description Post a new dataset to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the dataset in list format
addDataset <- function(api, data)
{
	data$owner <- whoAmI(api)
	data$networks <- multi_resToURI(api, data$networks, 'network')
	if(!is.null(data$papers)) data$papers <- multi_resToURI(api, data$papers, 'reference')
	if(!is.null(data$data)) data$data <- multi_resToURI(api, data$data, 'reference')
	mangalPost(api, 'dataset', data)
}

#' @title Patch a dataset
#' 
#' @description Patch a dataset from the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the dataset in list format
patchDataset <- function(api, data)
{
	data$owner <- whoAmI(api)
	data$networks <- multi_resToURI(api, data$networks, 'network')
	if(!is.null(data$papers)) data$papers <- multi_resToURI(api, data$papers, 'reference')
	if(!is.null(data$data)) data$data <- multi_resToURI(api, data$data, 'reference')
	mangalPost(api, 'dataset', data)
}