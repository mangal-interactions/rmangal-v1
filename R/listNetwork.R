#' List the networks currently in the DB
#'
#' Returns a list of networks
#'
#' @param api a \code{\link{mangalapi}} object
listNetwork <- function(api, ...)
{
	# ... will be updated to allow filtering in a future release
	if(!is.null(api$dataset))
	{
		return(pagerResources(api, httr::GET(api$network)))
	} else {
		stop("This API do not implement the listing of networks")
	}
}