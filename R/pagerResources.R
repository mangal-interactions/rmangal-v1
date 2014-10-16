#' Read resources
#'
#' Read a page list of resources
#'
#' @param api a \code{\link{mangalapi}} object
#' @param queryset the entry point in the API
pagerResources <- function(api, queryset)
{
	resources <- NULL
	if(httr::http_status(queryset)$category == "success")
	{
		fields <- httr::content(queryset)
		cat(stringr::str_c(fields$meta$total_count,' object(s) found\n'))
		while(!(is.null(fields$meta$`next`)))
		{
			resources <- c(resources, fields$objects)
			fields <- httr::content(httr::GET(stringr::str_c(api$base,fields$meta$`next`)))
		}
		resources <- c(resources, fields$objects)
		return(resources)
	} else {
		stop(httr::http_status(queryset)$message)
	}
}
