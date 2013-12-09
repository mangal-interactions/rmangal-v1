#' Create a new taxa
#'
#' Returns a list of datasets
#'
#' @param api a \code{\link{mangalapi}} object
#' @param object a list in the taxa format
addTaxa <- function(api, taxa)
{
   if(is.null(api$auth)) stop("You must be authenticated to post")
	if(!is.null(api$taxa))
	{
		datasets <- NULL
		queryset <- httr::GET(api$taxa, body = toJSON(taxa), add_headers("Content-type" = "application/json"), api$auth)
		if(http_status(queryset)$category == "success")
		{
			new_taxa <- content(queryset)
			return(new_taxa)
		} else {
			stop(http_status(queryset)$message)
		}
	} else {
		stop("This API do not implement the upload of taxa objects")
	}
   
}
