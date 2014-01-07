#' @title List the interactions currently in the DB
#' @export
#' 
#' @description Returns a list of interactions
#'
#' @param api a \code{\link{mangalapi}} object
listInteraction <- function(api) mangalList(api, 'interaction')

#' @title Get a particular interaction
#' @export
#' 
#' @description Returns a interaction object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a interaction
getInteraction <- function(api, id) mangalGet(api, 'interaction', id)

#' @title Add a new interaction
#' @export
#' 
#' @description Post a new interaction to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the interaction in list format
addInteraction <- function(api, data)
{
	data$taxa_from <- resToURI(api, data$taxa_from, 'taxa')
	data$taxa_to <- resToURI(api, data$taxa_to, 'taxa')
	if(!is.null(data$population_from)) data$population_from <- resToURI(api, data$population_from, 'population')
	if(!is.null(data$population_to)) data$population_to <- resToURI(api, data$population_to, 'population')
	if(!is.null(data$item_from)) data$item_from <- resToURI(api, data$item_from, 'item')
	if(!is.null(data$item_to)) data$item_to <- resToURI(api, data$item_to, 'item')
	mangalPost(api, 'interaction', data)
}

#' @title Patch an interaction
#' @export
#' 
#' @description Patch an interaction from the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the interaction in list format
patchInteraction <- function(api, data)
{
	data$taxa_from <- resToURI(api, data$taxa_from, 'taxa')
	data$taxa_to <- resToURI(api, data$taxa_to, 'taxa')
	if(!is.null(data$population_from)) data$population_from <- resToURI(api, data$population_from, 'population')
	if(!is.null(data$population_to)) data$population_to <- resToURI(api, data$population_to, 'population')
	if(!is.null(data$item_from)) data$item_from <- resToURI(api, data$item_from, 'item')
	if(!is.null(data$item_to)) data$item_to <- resToURI(api, data$item_to, 'item')
	mangalPatch(api, 'interaction', data)
}