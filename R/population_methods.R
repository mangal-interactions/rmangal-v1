#' @title List the populations
#'
#' @description Returns a list of populations
#'
#' @param api a \code{\link{mangalapi}} object
listPopulation <- function(api) mangalList(api, 'population')

#' @title Get a population
#'
#' @description Returns a population
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of the population
getPopulation <- function(api, id) mangalGet(api, 'population', id)

#' @title Add a new population
#' 
#' @description Post a new population to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the population in list format
addPopulation <- function(api, data)
{
	data$taxa <- resToURI(api, data$taxa, 'taxa')
	mangalPost(api, 'population', data)
}

#' @title Patch a population
#' 
#' @description Modify the informations for a population
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the population in list format
patchPopulation <- function(api, data)
{
	data$taxa <- resToURI(api, data$taxa, 'taxa')
	mangalPatch(api, 'population', data)
}