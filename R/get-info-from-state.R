#' Get informations from population state
#'
#' Get infos from state
#'
#' @export
#'
#' @param API A \code{\link{mangalAPI}} object
#' @param id The \code{id} of the population state
getInfosFromPopstate <- function(API, id)
{
   popstate <- getPopstate(API, id)
   population <- getPop(API, popstate$population)
   taxa <- getTaxa(API, population$taxa)
   taxa$population <- population
   return(taxa)
}
