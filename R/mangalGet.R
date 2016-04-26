#' @title Get an object of a given type
#'
#' Returns an object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type a type of object
#' @param id the ID of the object
mangalGet <- function(api, type, id = NULL)
{
	if(is.null(api[[type]]) | !("get" %in% api[[type]]$verbs)) stop(stringr::str_c("This API do not implement the listing of ",type))
	if(is.null(id)) stop("You must provide an ID")
	query_url <- stringr::str_c(api[[type]]$url,id,'/', render_parameters(api))
	queryset <- httr::GET(query_url)
	if(tolower(httr::http_status(queryset)$category) == "success")
	{
		fields <- httr::content(queryset)
      # This was NOT needed before...
      for(f in names(fields))
      {
         if(is.list(fields[[f]])) fields[[f]] <- unlist(fields[[f]])
      }
		return(fields)
	} else {
		stop(httr::http_status(queryset)$message)
	}
}
