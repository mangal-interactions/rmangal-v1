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
		networks <- NULL
		queryset <- httr::GET(api$network)
		if(http_status(queryset)$category == "success")
		{
			fields <- content(queryset)
			cat(paste(fields$meta$total_count,'object(s) found\n'))
			while(!(is.null(fields$meta$`next`)))
			{
				networks <- c(networks, fields$objects)
				fields = content(httr::GET(paste(api$base,fields$meta$`next`,sep='')))
			}
			networks <- c(networks, fields$objects)
			return(networks)
		} else {
			stop(http_status(queryset)$message)
		}
	} else {
		stop("This API do not implement the listing of networks")
	}
}