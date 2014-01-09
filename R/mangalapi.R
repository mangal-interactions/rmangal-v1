#' @title Initialise a mangal API object
#' @export
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
		methods$resources <- names(list_of_methods)
		for(res in methods$resources)
		{
			methods[[res]]$url <- paste(url,list_of_methods[[res]]$list_endpoint, sep='')
         methods[[res]]$verbs <- content(httr::GET(paste(url, list_of_methods[[res]]$schema, sep='')))$allowed_list_http_methods
		}
		if(!(is.null(usr) & is.null(pwd)))
		{
			us <- content(httr::GET(paste(methods$user$url,'?username=',methods$usr,sep='')))$objects[[1]]
			methods$me <- resToURI(methods, us, 'user')
		}
		return(methods)
	} else {
		stop(http_status(queryset)$message)
	}
}

#' @title Convert resource or ID to URI
#' 
#' @description Returns the URI of a resource, for internal use only
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param obj the object to convert
#' @param type the type of object to convert
resToURI <- function(api, obj, type)
{
	if(is.list(obj))
	{
		return(paste(api$trail, '/', type, '/', obj$id, '/', sep=''))
	} else {
		return(paste(api$trail, '/', type, '/', obj, '/', sep=''))
	}
}

#' @title Convert several resources or IDs to URI
#' 
#' @description For internal use only
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param obj the object to convert
#' @param type the type of object to convert
multi_resToURI <- function(api, obj, type)
{
	if(is.vector(obj))
	{
		obj <- aaply(obj, 1, function(x) resToURI(api, x, type))
		names(obj) <- NULL
		if(length(obj) == 1) obj <- list(obj)
		return(obj)
	}
	if(is.list(obj))
	{
		data$networks <- laply(obj, function(x) resToURI(api, x, type))
		return(obj)
	}
	stop("The resource you try to convert must be a vector or list")
}

#' @title Get self user info
#' 
#' @description Get self user info needed for paternity of data
#' 
#' @param api a \code{\link{mangalapi}} object
whoAmI <- function(api)
{
	if(is.null(api$auth)) stop("You must be logged in")
	return(api$me)
}

#' @title List the available resources
#' @export
#' 
#' @description Gives an array with the available resources
#' 
#' @param api a \code{\link{mangalapi}} object
availableResources <- function(api) names(api)

#' @title How should objects be formatted
#' @export
#' 
#' @description Prints a data.frame with informations about object format and help text
#'
#' @param api a \code{\link{mangalapi}} object
#' @param type the type of object you want to know about
#' @param ... researved for future use (export as JSON schemes)
whatIs <- function(api, type, ...)
{
	if(!(type %in% availableResources(api))) stop(paste("This API do not implement objects of type ",type,'. See ?availableResources for more.',sep=''))
	schema <- paste(api[[type]]$url,'schema',sep='')
	type_spec <- content(httr::GET(schema))
	# Print a data.frame with the fields
	spec <- ldply(type_spec$fields, summarize, help = help_text, type = type, null = as.character(nullable), unique = unique, values = ifelse(exists('choices'), paste(choices, collapse=', ') , ''))
	colnames(spec)[1] = 'field'
	# Remove owner, public and id
	spec = subset(spec, !(field %in% c('owner', 'id', 'public')))
	# Give more explicit messages (case by case)
	if(type == 'dataset')
	{
		spec[which(spec$field=='data'),'help'] = "A list of the id of references for the data (e.g. data papers, figshare dataset)"
		spec[which(spec$field=='networks'),'help'] = "A list of either the id of networks, or their representation in list format"
	}
	# Return the dataframe
	return(spec)
}
