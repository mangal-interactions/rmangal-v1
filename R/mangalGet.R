#' Get an object of a given type
#'
#' Returns an object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type a type of object
#' @param id the ID of the object
mangalGet <- function(api, type, id = NULL)
{
	if(is.null(api[[type]]) | !("get" %in% api[[type]]$verbs)) stop(paste("This API do not implement the listing of ",type,sep=''))
	if(is.null(id)) stop("You must provide an ID")
	queryset <- httr::GET(paste(api[[type]]$url,id,'/',sep=''))
	if(http_status(queryset)$category == "success")
	{
		fields <- content(queryset)
		return(fields)
	} else {
		stop(http_status(queryset)$message)
	}
}