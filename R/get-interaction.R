#' Get a particular interaction
#'
#' Returns an interaction from a network object
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param dataset A \code{list} object returned by \code{\link{getDataset}}
#' @param network A \code{list} object returned by \code{\link{getNetwork}}
#' @param id The \code{id} of the interaction to return
getInteraction <- function(API, dataset, network, id)
{
   if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
   l_ply(list(dataset, network), checkArg)
   if(!(network$id %in% dataset$networks)) stop("The network you want is not part of this dataset")
   if(!(id %in% network$interactions)) stop("The interaction you want do not belong to this network")
   list_int <- httr::GET(paste(API$url,'dataset', dataset$id, 'network', network$id, 'interaction', id, sep='/'))
   if(list_int$status_code == 200) return(content(list_int))
}
