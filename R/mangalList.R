#' List all objects of a given type
#'
#' Returns a list of objects of a given type
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type a type of object
#' @param filtering a vector of filters to restrict the returned objects
mangalList <- function(api, type, filtering=NULL)
{
	if(is.null(api[[type]]) | !("get" %in% api[[type]]$verbs)) stop(str_c("This API do not implement the listing of ",type))
	query_url <- str_c(api[[type]]$url, render_parameters(api, filtering))
	return(pagerResources(api, GET(query_url)))
}
