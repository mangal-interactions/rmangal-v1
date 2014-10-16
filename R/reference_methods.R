#' @title List the references
#' @export
#' 
#' @description Returns a list of references
#'
#' @param api a \code{\link{mangalapi}} object
#' @param ... additional parameters (filters) to be passed to \code{\link{mangalList}}
listReference <- function(api, ...) mangalList(api, 'reference', ...)

#' @title Get a reference
#' @export
#' 
#' @description Returns a reference
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of the reference
getReference <- function(api, id) mangalGet(api, 'reference', id)

#' @title Add a new reference
#' @export
#' 
#' @description Post a new reference to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the reference in list format
addReference <- function(api, data) mangalPost(api, 'reference', data)

#' @title Patch a reference
#' @export
#' 
#' @description Modify the informations for a reference
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the reference in list format
patchReference <- function(api, data) mangalPatch(api, 'reference', data)
