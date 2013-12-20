#' @title List the taxa currently in the DB
#'
#' @description Returns a list of taxa
#'
#' @param api a \code{\link{mangalapi}} object
listTaxa <- function(api) mangalList(api, 'taxa')

#' @title Get a particular taxa
#'
#' @description Returns a taxa object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a taxa
getTaxa <- function(api, id) mangalGet(api, 'taxa', id)

#' @title Add a new taxa object
#' 
#' @description Post a new taxa to the database
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the taxa in list format
addTaxa <- function(api, data) mangalPost(api, 'taxa', data)

#' @title Patch a taxa
#' 
#' @description Modify the informations for a taxa
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the taxa in list format
patchTaxa <- function(api, data) mangalPatch(api, 'taxa', data)