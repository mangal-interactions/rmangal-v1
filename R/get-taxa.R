#' Get a specific taxa
#'
#' Return a list with the informations on a specific taxa
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param id A string giving the id of the taxa
getTaxa <- function(API, id)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  if(!is.character(id)) stop("The id argument must be a string")
  taxa <- httr::GET(paste(API$url,'taxa',id,sep='/'))
  if(taxa$status_code == 404) stop("This taxa cannot be found in the database, use listTaxa for a list of taxa")
  if(taxa$status_code == 200) return(content(taxa))
}