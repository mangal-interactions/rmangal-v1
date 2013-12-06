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
		datasets <- NULL
		queryset <- httr::GET(api$dataset)
		if(http_status(queryset)$category == "success")
		{
			fields <- content(queryset)
			cat(paste(fields$meta$total_count,'object(s) found\n'))
			while(!(is.null(fields$meta$`next`)))
			{
				datasets <- c(datasets, fields$objects)
				fields = content(httr::GET(paste(api$base,fields$meta$`next`,sep='')))
			}
			datasets <- c(datasets, fields$objects)
			return(datasets)
		} else {
			stop(http_status(queryset)$message)
		}
	} else {
		stop("This API do not implement the listing of datasets")
	}
   
}
