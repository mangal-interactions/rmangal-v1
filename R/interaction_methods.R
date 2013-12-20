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

#' @title Add a new interaction
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
	mangalPost(api, 'interaction', data)
}