#' @title List the networks currently in the DB
#' @export
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

#' @title Add a new network
#' @export
#' 
#' @description Post a new network to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the network in list format
addNetwork <- function(api, data)
{
	data$owner <- whoAmI(api)
	data$interactions <- multi_resToURI(api, data$interactions, 'interaction')
	mangalPost(api, 'network', data)
}

#' @title Patch a network
#' @export
#' 
#' @description Modify the informations for a network
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the network in list format
patchNetwork <- function(api, data)
{
	data$owner <- whoAmI(api)
	data$interactions <- multi_resToURI(api, data$interactions, 'interaction')
	mangalPatch(api, 'network', data)
}