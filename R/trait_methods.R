#' @title List the traits
#' @export
#' 
#' @description Returns a list of traits
#'
#' @param api a \code{\link{mangalapi}} object
#' @param ... additional parameters (filters) to be passed to \code{\link{mangalList}}
listTrait <- function(api, ...) mangalList(api, 'trait', ...)

#' @title Get an traital measure
#' @export
#'
#' @description Returns a list of traital measures
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of the trait
getTrait <- function(api, id) mangalGet(api, 'trait', id)

#' @title Add a new trait
#' @export
#' 
#' @description Post a new trait to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the trait in list format
addTrait <- function(api, data) mangalPost(api, 'trait', data)

#' @title Patch a trait
#' @export
#' 
#' @description Modify the informations for an trait
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the trait in list format
patchTrait <- function(api, data) mangalPatch(api, 'trait', data)
