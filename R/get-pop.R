#' Get a specific population
#'
#' Return a list with the informations on a specific population
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param id A string giving the id of the population
getPop <- function(API, id)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  pop <- httr::GET(paste(API$url,'taxa', 0,'pop',id,sep='/'))
  if(pop$status_code == 404) stop("This population cannot be found in the database, use listPop for a list of populations")
  if(pop$status_code == 200) return(content(pop))
}
