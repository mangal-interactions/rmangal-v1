#' List all objects of a given type
#'
#' Returns a list of objects of a given type
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type a type of object
mangalList <- function(api, type)
{
	if(is.null(api[[type]]) | !("get" %in% api[[type]]$verbs)) stop(paste("This API do not implement the listing of ",type,sep=''))
	return(pagerResources(api, httr::GET(api[[type]]$url)))
}
