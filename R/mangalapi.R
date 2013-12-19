#' @title Initialise a mangal API object
#'
#' @description Initialise a mangal API obect, with direct reference to the resources
#'
#' @param url The URL to the server
#' @param v The API version
#' @param usr The username
#' @param pwd The password
mangalapi <- function(url = "http://mangal.uqar.ca", v = 'v1', usr = NULL, pwd = NULL)
{
	queryset <- httr::GET(paste(url, 'api', v, sep='/'))
	if(http_status(queryset)$category == "success")
	{
		methods <- list()
      if(!(is.null(usr))&is.null(pwd)) warning("No password has been provided")
      if(!(is.null(pwd))&is.null(usr)) warning("No username has been provided")
      if(!(is.null(usr) & is.null(pwd)))
      {
      	methods$auth <- authenticate(usr, pwd, 'basic')
      	methods$usr <- usr
      }
		methods$base <- url
		methods$trail <- paste('/api', v, sep='/')
		list_of_methods <- content(queryset)
		for(i in c(1:length(list_of_methods)))
		{
			methods[[names(list_of_methods)[i]]]$url <- paste(url,list_of_methods[[i]]$list_endpoint, sep='')
         methods[[names(list_of_methods)[i]]]$verbs <- content(httr::GET(paste(url, list_of_methods[[i]]$schema, sep='')))$allowed_list_http_methods
		}
		return(methods)
	} else {
		stop(http_status(queryset)$message)
	}
}

#' @title Get self user info
#' 
#' @description Get self user info needed for paternity of data
#' 
#' @param api a \code{\link{mangalapi}} object
whoAmI <- function(api)
{
	if(is.null(api$auth)) stop("You must be logged in")
	return(content(httr::GET(paste(api$user$url,'?username=',api$usr,sep='')))$objects[[1]])
}