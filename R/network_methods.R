#' @title List the networks currently in the DB
#' @export
#' 
#' @description Returns a list of networks
#'
#' @param api a \code{\link{mangalapi}} object
#' @param ... additional parameters (filters) to be passed to \code{\link{mangalList}}
listNetwork <- function(api, ...) mangalList(api, 'network', ...)

#' @title Get a particular network
#' @export
#' 
#' @description Returns a network object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a network
getNetwork <- function(api, id)
{
  network <- mangalGet(api, 'network', id)
  for(coord in c('latitude', 'longitude'))
  {
    # I shouldn't be doing that..
    if(!is.null(network[[coord]])) network[[coord]] <- as.numeric(network[[coord]])
  }
  return(network)
}

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
	data$interactions <- multi_resToURI(api, data$interactions, 'interaction')
	if(!is.null(data$paper)) data$paper <- multi_resToURI(api, data$paper, 'reference')
	if(!is.null(data$data)) data$data <- multi_resToURI(api, data$data, 'reference')
   if(!is.null(data$environment)) data$environment <- multi_resToURI(api, data$environment, 'environment')
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
	data$interactions <- multi_resToURI(api, data$interactions, 'interaction')
	if(!is.null(data$paper)) data$paper <- multi_resToURI(api, data$paper, 'reference')
	if(!is.null(data$data)) data$data <- multi_resToURI(api, data$data, 'reference')
   if(!is.null(data$environment)) data$environment <- multi_resToURI(api, data$environment, 'environment')
	mangalPatch(api, 'network', data)
}
