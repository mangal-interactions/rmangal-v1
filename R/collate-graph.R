#' Collate graph
#'
#' Returns a network as an \code{\link{igraph}} object
#' @export
#'
#' @param API An \code{\link{mangalAPI}} object
#' @param dataset A \code{list} object with the dataset information
#' @param network A \code{list} object with the network information
collateGraph <- function(API, dataset, network, ...)
{
   edgelist <- NULL
   #NOTE the ... is for later use
   l_ply(list(dataset, network), checkArg)
   # List of interactions
   all_ints <- listInteractions(API, dataset, network)
   for(each_int in all_ints)
   {
      pfrom <- getInfosFromPopstate(API, each_int$pop_from)
      pto <- getInfosFromPopstate(API, each_int$pop_to)
      pf_name <- paste(pfrom$name, pfrom$population$name)
      pt_name <- paste(pto$name, pto$population$name)
      edgelist <- rbind(edgelist, c(pf_name, pt_name))
   }
   print(edgelist)
   return(graph.data.frame(edgelist))
}
