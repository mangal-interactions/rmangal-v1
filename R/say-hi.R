#' Says Hi! to the API
#'
#' This function tries to connect to a mangal API.
#' If this is a success, it returns the response for the
#' API top level adress (giving, most notably, the API
#' version number and codename).
#' 
#' @export
#' 
#' @param API An object of class \code{\link{mangalAPI}}
sayHi <- function(API)
{
   hello_message <- httr::GET(API$url)
   if(hello_message$status_code == 200) return(content(hello_message))
   stop("Unable to reach the API")
}