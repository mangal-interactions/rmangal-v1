#' @title List the references
#'
#' @description Returns a list of references
#'
#' @param api a \code{\link{mangalapi}} object
listReference <- function(api) mangalList(api, 'reference')

#' @title Get a reference
#' 
#' @description Returns a reference
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of the reference
getReference <- function(api, id) mangalGet(api, 'reference', id)

#' @title Add a new reference
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
#' 
#' @description Modify the informations for a reference
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the reference in list format
patchReference <- function(api, data) mangalPatch(api, 'reference', data)
