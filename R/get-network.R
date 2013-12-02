#' Get a particular network
#'
#' Returns a network object from a dataset
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param dataset A \code{list} object returned by \code{\link{getDataset}}
#' @param id The \code{id} of the network to return
getNetwork <- function(API, dataset, id)
{
   if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
   checkArg(dataset)
   if(!(id %in% dataset$networks)) stop("The network you want is not part of this dataset")
   list_net <- httr::GET(paste(API$url,'dataset', dataset$id, 'network', id, sep='/'))
   if(list_net$status_code == 200) return(content(list_net))
}
