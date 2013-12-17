#' Initialise a mangal API object
#'
#' Initialise a mangal API obect, with direct reference to the resources
#'
#' @param url The URL to the server
#' @param v The API version
#' @param usr The username
#' @param pwd The password
mangalapi <- function(url = "http://mangal.uqar.ca", v = 'v1', usr = NULL, pwd = NULL, ...)
{
	# ... will be updated for authentication in a future release
	queryset <- httr::GET(paste(url, 'api', v, sep='/'))
	if(http_status(queryset)$category == "success")
	{
		methods <- list()
      if(!(is.null(usr))&is.null(pwd)) warning("No password has been provided")
      if(!(is.null(pwd))&is.null(usr)) warning("No username has been provided")
      if(!(is.null(usr) & is.null(pwd))) methods$auth <- authenticate(user, pwd, 'basic')
		methods$base <- url
		list_of_methods <- content(queryset)
		for(i in c(1:length(list_of_methods)))
		{
			methods[[names(list_of_methods)[i]]] <- paste(url,list_of_methods[[i]]$list_endpoint, sep='')
		}
		return(methods)
	} else {
		stop(http_status(queryset)$message)
	}
}
