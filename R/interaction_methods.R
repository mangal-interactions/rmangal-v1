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
	if(!(is.list(data$taxa_from))) data$taxa_from = getTaxa(api, data$taxa_from)
	if(!(is.list(data$taxa_to))) data$taxa_to = getTaxa(api, data$taxa_to)
	if(!is.null(data$population_from)) if(!(is.list(data$population_from))) data$population_from = getPopulation(api, data$population_from)
	if(!is.null(data$population_to)) if(!(is.list(data$population_to))) data$population_to = getPopulation(api, data$population_to)
	if(!is.null(data$item_from)) if(!(is.list(data$item_from))) data$item_from = getItem(api, data$item_from)
	if(!is.null(data$item_to)) if(!(is.list(data$item_to))) data$item_to = getItem(api, data$item_to)
	mangalPost(api, 'interaction', data)
}