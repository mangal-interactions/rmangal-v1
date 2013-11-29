#' Handles connection to the database
#'
#' This function returns an object of class \code{mangalAPI},
#' that has the URL of the API and possibly the username /
#' password informations.
#'
#' @export
#'
#' @param api URL of the API -- default to the UQAR API
#' @param usr Username on the API server
#' @param pwd Password
mangalAPI <- function(api = "http://localhost:8000/api", usr = NULL, pwd = NULL)
{
   if(is.null(api)) stop("The API URL cannot be empty") 
   if(is.null(usr) & !is.null(pwd)) warning("No username has been provided")
   if(is.null(pwd) & !is.null(usr)) warning("No password has been provided")
   connection_info <- list(url = api, usr = usr, pwd = pwd)
   class(connection_info) <- "mangal"
   return(connection_info)
}