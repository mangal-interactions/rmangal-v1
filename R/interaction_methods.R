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
	mangalPost(api, 'interaction', data)
}