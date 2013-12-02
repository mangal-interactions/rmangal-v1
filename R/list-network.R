#' Lists the networks that are part of a dataset
#'
#' Makes a list of all networks part of a dataset
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param dataset A \code{list} object representing a dataset
listNetworks <- function(API, dataset)
{
   if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
   checkArg(dataset)
   if(length(dataset$networks) == 0) stop("This dataset currently has no networks")
   list_net <- httr::GET(paste(API$url,'dataset', dataset$id, 'network', sep='/'))
   if(list_net$status_code == 200) return(content(list_net))
}
