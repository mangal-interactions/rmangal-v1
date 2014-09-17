#' @title Release a resource
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param type the type of object to release
#' @param id the id of the dataset to release
#'
#' @description 
#' This function is used internally to release a resource. It do not returns
#' anything, but throws a warning if there is no public field to update.
releaseResource <- function(api, type, id)
{
   resource <- mangalGet(api, type, id)
   if(is.null(resource$public))
   {
      warning(stringr::str_c("The resource ", type, '/', id, " has no public field"))
   } else {
      resource$public <- TRUE
      mangalPatch(api, type, resource)
   }
}

#' @title Release a dataset
#' @description 
#' Releases a dataset, and all objects of lower level (networks and 
#' interactions).
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the id of the dataset to release
#' @param force whether to force release even if the dataset is already public
#' 
#' @export
releaseDataset <- function(api, id, force=FALSE)
{
   dataset <- getDataset(api, id)
   if(dataset$public & (! force)) stop("This dataset is already public") 
   # We go through 1) the list of networks, 2) the list of interactions
   for(net_id in dataset$networks)
   {
      net <- getNetwork(api, net_id)
      releaseResource(api, 'network', net_id)
      for(int_id in net$interactions)
      {
         releaseResource(api, 'interaction', int_id)
      }
   }
   releaseResource(api, 'dataset', id)
}

