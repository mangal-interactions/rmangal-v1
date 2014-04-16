#' List all objects of a given type
#'
#' Returns a list of objects of a given type
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type a type of object
#' @param filtering filters to restrict the returned objects
mangalList <- function(api, type, filtering=NULL)
{
	if(is.null(api[[type]]) | !("get" %in% api[[type]]$verbs)) stop(paste("This API do not implement the listing of ",type,sep=''))
	query_url <- str_c(api[[type]]$url, '?', api$auth)
   query_url <- ifelse(is.null(filtering), query_url, str_c(query_url,'&',filtering))
	return(pagerResources(api, GET(query_url)))
}
