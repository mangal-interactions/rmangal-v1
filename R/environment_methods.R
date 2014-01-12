#' @title List the environments
#' @export
#' 
#' @description Returns a list of environments
#'
#' @param api a \code{\link{mangalapi}} object
listEnvironment <- function(api) mangalList(api, 'environment')

#' @title Get an environmental measure
#' @export
#'
#' @description Returns a list of environmental measures
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of the environment
getEnvironment <- function(api, id) mangalGet(api, 'environment', id)

#' @title Add a new environment
#' @export
#' 
#' @description Post a new environment to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the environment in list format
addEnvironment <- function(api, data) mangalPost(api, 'environment', data)

#' @title Patch a environment
#' @export
#' 
#' @description Modify the informations for an environment
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the environment in list format
patchEnvironment <- function(api, data) mangalPatch(api, 'environment', data)
