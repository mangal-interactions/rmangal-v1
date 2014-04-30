#' @title List the taxa currently in the DB
#' @export
#' 
#' @description Returns a list of taxa
#'
#' @param api a \code{\link{mangalapi}} object
#' @param ... additional parameters (filters) to be passed to \code{\link{mangalList}}
listTaxa <- function(api, ...) mangalList(api, 'taxa', ...)

#' @title Get a particular taxa
#' @export
#' 
#' @description Returns a taxa object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a taxa
getTaxa <- function(api, id) mangalGet(api, 'taxa', id)

#' @title Add a new taxa object
#' @export
#' 
#' @description Post a new taxa to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the taxa in list format
addTaxa <- function(api, data)
{
	if(!is.null(data$traits)) data$traits <- multi_resToURI(api, data$traits, 'trait')
	mangalPost(api, 'taxa', data)
}

#' @title Patch a taxa
#' @export
#' 
#' @description Modify the informations for a taxa
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the taxa in list format
patchTaxa <- function(api, data)
{
	if(!is.null(data$traits)) data$traits <- multi_resToURI(api, data$traits, 'trait')
	mangalPatch(api, 'taxa', data)
}
