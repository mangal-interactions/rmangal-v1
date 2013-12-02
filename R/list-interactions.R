#' Lists the interactions within a network
#'
#' Makes a list of all interactions part of a network
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param dataset A \code{list} object representing a dataset
#' @param network A \code{list} object representing a network
listNetworks <- function(API, dataset, network)
{
   if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
   a_ply(c(dataset, network), 1, checkArg)
   if(!(network$id %in% dataset$networks)) stop("The network you look for do not belong to the dataset")
   if(length(network$interactions) == 0) stop("This network currently has no interactions")
   list_int <- httr::GET(paste(API$url,'dataset', dataset$id, 'network', network$id, 'interaction', sep='/'))
   if(list_int$status_code == 200) return(content(list_int))
}
