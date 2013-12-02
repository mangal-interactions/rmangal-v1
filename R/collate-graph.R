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
   gr <- graph.empty()
   #NOTE the ... is for later use
   a_ply(c(dataset, network), 1, checkArg)
   # List of interactions
   all_ints <- listInteraction(API, dataset, network)
   for(each_int in all_ints)
   {
      pfrom <- getInfosFromPopstate(each_int$pop_from)
      pto <- getInfosFromPopstate(each_int$pop_to)
      pf_name <- paste(pfrom$name, pfrom$population$name)
      pt_name <- paste(pto$name, pto$population$name)
      gr <- add.edges(gr, c(pf_name, pt_name))
   }
   return(gr)
}
