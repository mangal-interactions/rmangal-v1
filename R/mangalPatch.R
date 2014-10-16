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
	if(is.null(api[[type]]) | !("patch" %in% api[[type]]$verbs)) stop(stringr::str_c("This API do not permit PATCHing objects of type ",type))
	if(!type == 'user') data$owner <- uri_from_uname(api, data$owner)
	qURL <- stringr::str_c(api[[type]]$url, data$id)
	if(!(stringr::str_sub(qURL,-1)=='/')) qURL <- stringr::str_c(qURL,'/',sep='')
   # We need to get rid of everything NULL, or else jsonlite will screw it up
   data[names(data)[plyr::laply(data, length)==0]] <- NULL
	queryset <- httr::PATCH(stringr::str_c(qURL, '?', api$auth), body = jsonlite::toJSON(data, auto_unbox=TRUE), httr::add_headers("Content-type" = "application/json"))
	if(httr::http_status(queryset)$category == "success")
	{
		new_entry <- httr::content(queryset)
		return(new_entry)
	} else {
		print(httr::content(queryset))
		stop(httr::http_status(queryset)$message)
	}
}

#' @title Get the URI of a user from its username
#'
#' @param api a \code{\link{mangalapi}} object
#' @param uname a username
#' @export
uri_from_uname <- function(api, uname)
{
   filter <- stringr::str_c('username__exact=', uname)
   matches <- mangalList(api, 'user', filtering=filter)
   if (length(matches) == 0)
   {
      stop(stringr::str_c(uname, " is not a valid username"))
   }
   return(resToURI(api, matches[[1]], 'user'))
}
