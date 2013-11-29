#' Lists the datasets in the DB
#'
#' Makes a list of all datasets currently in the database
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
listDatasets <- function(API)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  list_ds <- httr::GET(paste(API$url,'dataset', sep='/'))
  if(list_pop$status_code == 200) return(content(list_ds))
}
