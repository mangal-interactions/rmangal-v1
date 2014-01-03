#' @title Get self user info
#'
#' @description Returns a user object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a network
getUser <- function(api)
{
	if(is.null(api$auth)) stop("You need to be authenticated")
	mangalGet(api, 'user', tail(strsplit(whoAmI(api),'/')[[1]],1))
}

#' @title Sign-up
#' 
#' @description Register as a new user
#' 
#' @details
#' Pick a good password!
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param usr a username
#' @param password
signUp <- function(api, usr, pwd) mangalPost(api, 'user', list(username=usr, password=pwd))

#' @title Patch self user info
#' 
#' @description Modify my user info
#' 
#' @details
#' Requires authentication
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param data the network in list format
patchUser <- function(api, data) mangalPatch(api, 'user', data)