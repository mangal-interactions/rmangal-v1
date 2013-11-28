#' Lists the taxa in the database
#'
#' Makes a list of all available taxa in the queried database.
#'
#' @param API An object of class \code{\link{mangal}}
listTaxa <- function(API)
{
  list_taxa <- httr::GET(paste(API$url,'taxa',sep='/'))
  if(list_taxa$status_code == 200) return(content(list_taxa))
}