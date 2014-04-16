#' @title List the interactions currently in the DB
#' @export
#' 
#' @description Returns a list of interactions
#'
#' @param api a \code{\link{mangalapi}} object
#' @param ... additional parameters (filters) to be passed to \code{\link{mangalList}}
listInteraction <- function(api, ...) mangalList(api, 'interaction', ...)

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
#' If \code{item_*} or \code{pop_*} are provided in \code{data}, it is not necessary to fill-in other top-level fields (\code{taxa_*} and \code{pop_*}). If they are not filled, they will be retrieved by the package (at the cost of a few additional queries, which might rapidly add-up for large datasets).
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the interaction in list format
addInteraction <- function(api, data)
{
	if(!is.null(data$taxa_from)) data$taxa_from <- resToURI(api, data$taxa_from, 'taxa')
	if(!is.null(data$taxa_to)) data$taxa_to <- resToURI(api, data$taxa_to, 'taxa')
	if(!is.null(data$environment)) data$environment <- multi_resToURI(api, data$environment, 'environment')
	# Auto-fill informations if only populations are given
	if(!is.null(data$pop_from))
	{
		if(is.null(data$taxa_from)) data$taxa_from <- resToURI(api, pop_from$taxa, 'taxa')
		data$pop_from <- resToURI(api, data$pop_from, 'population')
	}
	if(!is.null(data$pop_to))
	{
		if(is.null(data$taxa_to)) data$taxa_to <- resToURI(api, pop_to$taxa, 'taxa')
		data$pop_to <- resToURI(api, data$pop_to, 'population')
	}
	# Auto-fill informations if the items are given
	if(!is.null(data$item_from))
	{
		if(is.null(data$pop_from) | is.null(data$taxa_from)) pop_from <- getPopulation(api, ifelse(is.list(data$item_from), data$item_from$id, data$item_from))
		if(is.null(data$pop_from)) data$pop_from <- resToURI(api, pop_from$id, 'population')
		if(is.null(data$taxa_from)) data$taxa_from <- resToURI(api, pop_from$taxa, 'taxa')
		data$item_from <- resToURI(api, data$item_from, 'item')
	}
	if(!is.null(data$item_to))
	{
		if(is.null(data$pop_to) | is.null(data$taxa_to)) pop_to <- getPopulation(api, ifelse(is.list(data$item_to), data$item_to$id, data$item_to))
		if(is.null(data$pop_to)) data$pop_to <- resToURI(api, pop_to$id, 'population')
		if(is.null(data$taxa_to)) data$taxa_to <- resToURI(api, pop_to$taxa, 'taxa')
		data$item_to <- resToURI(api, data$item_to, 'item')
	}
	mangalPost(api, 'interaction', data)
}

#' @title Patch an interaction
#' @export
#' 
#' @description Patch an interaction from the database
#' 
#' @details
#' Requires authentication.
#' 
#' If \code{item_*} or \code{pop_*} are provided in \code{data}, it is not necessary to fill-in other top-level fields (\code{taxa_*} and \code{pop_*}). If they are not filled, they will be retrieved by the package (at the cost of a few additional queries, which might rapidly add-up for large datasets).
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the interaction in list format
patchInteraction <- function(api, data)
{
	if(!is.null(data$taxa_from)) data$taxa_from <- resToURI(api, data$taxa_from, 'taxa')
	if(!is.null(data$taxa_to)) data$taxa_to <- resToURI(api, data$taxa_to, 'taxa')
	if(!is.null(data$environment)) data$environment <- multi_resToURI(api, data$environment, 'environment')
	# Auto-fill informations if only populations are given
	if(!is.null(data$pop_from))
	{
		if(is.null(data$taxa_from)) data$taxa_from <- resToURI(api, pop_from$taxa, 'taxa')
		data$pop_from <- resToURI(api, data$pop_from, 'population')
	}
	if(!is.null(data$pop_to))
	{
		if(is.null(data$taxa_to)) data$taxa_to <- resToURI(api, pop_to$taxa, 'taxa')
		data$pop_to <- resToURI(api, data$pop_to, 'population')
	}
	# Auto-fill informations if the items are given
	if(!is.null(data$item_from))
	{
		if(is.null(data$pop_from) | is.null(data$taxa_from)) pop_from <- getPopulation(api, ifelse(is.list(data$item_from), data$item_from$id, data$item_from))
		if(is.null(data$pop_from)) data$pop_from <- resToURI(api, pop_from$id, 'population')
		if(is.null(data$taxa_from)) data$taxa_from <- resToURI(api, pop_from$taxa, 'taxa')
		data$item_from <- resToURI(api, data$item_from, 'item')
	}
	if(!is.null(data$item_to))
	{
		if(is.null(data$pop_to) | is.null(data$taxa_to)) pop_to <- getPopulation(api, ifelse(is.list(data$item_to), data$item_to$id, data$item_to))
		if(is.null(data$pop_to)) data$pop_to <- resToURI(api, pop_to$id, 'population')
		if(is.null(data$taxa_to)) data$taxa_to <- resToURI(api, pop_to$taxa, 'taxa')
		data$item_to <- resToURI(api, data$item_to, 'item')
	}
	mangalPatch(api, 'interaction', data)
}