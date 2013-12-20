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

#' @title Add a new network
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
	if(is.vector(data$interactions))
	{
		data$interactions <- aaply(data$interactions, 1, function(x) resToURI(api, x, 'interaction'))
		names(data$interactions) <- NULL
	}
	if(is.list(data$interactions)) data$interactions <- laply(data$interactions, function(x) resToURI(api, x, 'interaction'))
	mangalPost(api, 'network', data)
}