#' List the datasets currently in the DB
#'
#' Returns a list of datasets
#'
#' @param api a \code{\link{mangalapi}} object
listDataset <- function(api, ...)
{
   # ... will be updated to allow filtering in a future release
	if(!is.null(api$dataset))
	{
		return(pagerResources(api, httr::GET(api$dataset)))
	} else {
		stop("This API do not implement the listing of datasets")
	}
}
