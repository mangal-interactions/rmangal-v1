#' Get a specific dataset
#'
#' Return a list with the informations on the dataset
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param id A string giving the id of the dataset
getDataset <- function(API, id)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  if(!is.character(id)) stop("The id argument must be a string")
  ds <- httr::GET(paste(API$url,'dataset',id,sep='/'))
  if(ds$status_code == 404) stop("This dataset cannot be found in the database, use listDataset for a list of datasets")
  if(ds$status_code == 200) return(content(ds))
}
