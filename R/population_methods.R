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
	data$taxa = getTaxa(api, data$taxa)
	mangalPost(api, 'population', data)
}