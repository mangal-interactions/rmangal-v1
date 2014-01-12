#' List all objects of a given type
#'
#' Returns a list of objects of a given type
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type a type of object
mangalList <- function(api, type, filtering=NULL)
{
	if(is.null(api[[type]]) | !("get" %in% api[[type]]$verbs)) stop(paste("This API do not implement the listing of ",type,sep=''))
   query_url <- ifelse(is.null(filtering), api[[type]]$url, str_c(api[[type]]$url,'&',filtering))
	return(pagerResources(api, GET(query_url)))
}
