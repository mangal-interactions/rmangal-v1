#' Handles connection to the database
#'
#' This function returns an object of class \code{mangal},
#' that has the URL of the API and possibly the username /
#' password informations.
#'
#' @param api URL of the API -- default to the UQAR API
#' @param user Username on the API server
#' @param pwd Password
mangal <- function(api = "http://localhost/8080/api", user = NULL, pwd = NULL)
{
   if(is.null(api)) stop("The API URL cannot be empty") 
   if(is.null(user) & !is.null(pwd)) warning("No username has been provided")
   if(is.null(pwd) & !is.null(user)) warning("No password has been provided")
   connection_info <- list(url = api, usr = user, pwd = pwd)
   class(connection_info) <- "mangal"
   return(connection_info)
}