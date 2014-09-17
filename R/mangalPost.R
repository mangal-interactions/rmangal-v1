#' Post an object
#'
#' Add a new object to the database
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type the type of object to add
#' @param data the object in list form
mangalPost <- function(api, type, data)
{
  if(is.null(api$auth)) stop("You must be authenticated to post")
  if(is.null(data)) stop("Please provide data to add to the database")
  if(is.null(api[[type]]) | !("post" %in% api[[type]]$verbs)) stop(stringr::str_c("This API do not permit POSTing objects of type ",type))
	if(!type == 'user') data$owner <- api$me
	queryset <- httr::POST(stringr::str_c(api[[type]]$url, render_parameters(api)), body = jsonlite::toJSON(data, auto_unbox=TRUE), httr::add_headers("Content-type" = "application/json"))
	if(httr::http_status(queryset)$category == "success")
	{
		new_entry <- httr::content(queryset)
		return(new_entry)
	} else {
		print(httr::content(queryset))
		stop(httr::http_status(queryset)$message)
	}
}
