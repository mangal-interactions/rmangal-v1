#' Lists the taxa in the database
#'
#' Makes a list of all available taxa in the queried database.
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
listTaxa <- function(API)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  list_taxa <- httr::GET(paste(API$url,'taxa',sep='/'))
  if(list_taxa$status_code == 200) return(content(list_taxa))
}