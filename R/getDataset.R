#' Get a particular dataset
#'
#' Returns a dataset object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id the identifier of a dataset
getDataset <- function(api, id)
{
	if(!is.null(api$dataset))
	{
		datasets <- NULL
		queryset <- httr::GET(paste(api$dataset,id,'/',sep=''))
		if(http_status(queryset)$category == "success")
		{
			fields <- content(queryset)
			return(fields)
		} else {
			stop(http_status(queryset)$message)
		}
	} else {
		stop("This API do not implement the listing of datasets")
	}
}