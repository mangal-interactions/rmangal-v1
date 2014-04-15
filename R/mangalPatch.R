#' @title Patch an object
#'
#' Patch an existing object
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type the type of object to patch
#' @param data the object in list form
mangalPatch <- function(api, type, data)
{
	if(is.null(api$auth)) stop("You must be authenticated to patch")
	if(is.null(data)) stop("Please provide data to patch the database")
	if(is.null(data$id)) stop("The ID field must be present to patch")
	if(is.null(api[[type]]) | !("patch" %in% api[[type]]$verbs)) stop(paste("This API do not permit PATCHing objects of type ",type,sep=''))
	if(!type == 'user') data$owner <- whoAmI(api)
	qURL <- paste(api[[type]]$url, data$id, sep='')
	if(!(str_sub(qURL,-1)=='/')) qURL <- paste(qURL,'/',sep='')
	queryset <- PATCH(str_c(qURL, '?', api$auth), body = rjson::toJSON(data), add_headers("Content-type" = "application/json"), api$auth)
	if(http_status(queryset)$category == "success")
	{
		new_entry <- content(queryset)
		return(new_entry)
	} else {
		print(content(queryset))
		stop(http_status(queryset)$message)
	}
}
