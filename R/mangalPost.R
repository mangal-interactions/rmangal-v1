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
  if(is.null(api[[type]]) | !("post" %in% api[[type]]$verbs)) stop(paste("This API do not permit POSTing objects of type ",type,sep=''))
	if(!type == 'user') data$owner <- api$me
	queryset <- POST(str_c(api[[type]]$url, '?', api$auth), body = toJSON(data), add_headers("Content-type" = "application/json"))
	if(http_status(queryset)$category == "success")
	{
		new_entry <- content(queryset)
		return(new_entry)
	} else {
		print(content(queryset))
		stop(http_status(queryset)$message)
	}
}
