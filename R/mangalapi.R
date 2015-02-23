#' @title Initialise a mangal API object
#' @export
#'
#' @description Initialise a mangal API obect, with direct reference to the resources
#'
#' @details
#'
#' This function establishes a connection to the mangal API located at the given url.
#' If the author has an username / API key pair, it can be stated here. Note that if this
#' information is in the `mangal_usr` and `mangal_key` options, it will be filled
#' unless a usr and key are supplied.
#'
#' @param url The URL to the server
#' @param v The API version
#' @param usr The username
#' @param key The API key
mangalapi <- function(url = "http://mangal.io", v = 'v1', usr = NULL, key = NULL)
{
  if(stringr::str_sub(url, start=-1) == '/') url <- stringr::str_sub(url, end=-2)
  queryset <- httr::GET(stringr::str_c(url, 'api', v, sep='/'))
  if(httr::http_status(queryset)$category == "success")
  {
    methods <- list()
    methods$args <- list(client = 'rmangal') # Additional URL parameters
    methods$auth <- FALSE
    if(is.null(usr)) usr <- options()$mangal_usr
    if(is.null(key)) key <- options()$mangal_key
    if(!(is.null(usr))&is.null(key)) warning("No password has been provided")
    if(!(is.null(key))&is.null(usr)) warning("No API key has been provided")
    if(!(is.null(usr) & is.null(key)))
    {
      methods$args$username <- usr
      methods$args$api_key <- key
      methods$auth <- TRUE
      methods$usr <- usr
    }
    methods$base <- url
    methods$trail <- stringr::str_c('/api', v, sep='/')
    list_of_methods <- httr::content(queryset)
    methods$resources <- names(list_of_methods)
    for(res in methods$resources)
    {
      methods[[res]]$url <- stringr::str_c(url,list_of_methods[[res]]$list_endpoint)
      methods[[res]]$verbs <- httr::content(httr::GET(stringr::str_c(url, list_of_methods[[res]]$schema)))$allowed_list_http_methods
    }
    if(methods$auth)
    {
      us <- httr::content(httr::GET(stringr::str_c(methods$user$url, render_parameters(methods, suppl=list('username__exact' = methods$usr)))))$objects[[1]]
      methods$me <- resToURI(methods, us, 'user')
    }
      return(methods)
    } else {
      stop(httr::http_status(queryset)$message)
  }
}

#' @title Render url additional key/value pairs
#' @param api a \code{\link{mangalapi}} object
#' @param suppl the additional parameters as a list (with names)
render_parameters <- function(api, suppl=NULL)
{
   full_args <- c(api$args, suppl)
   return(stringr::str_c('?',stringr::str_c(names(full_args), unlist(full_args), sep='=', collapse='&')))
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
    return(stringr::str_c(api$trail, '/', type, '/', obj$id, '/'))
	} else {
    return(stringr::str_c(api$trail, '/', type, '/', obj, '/'))
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
		obj <- plyr::aaply(obj, 1, function(x) resToURI(api, x, type))
		names(obj) <- NULL
		if(length(obj) == 1) obj <- list(obj)
		return(obj)
	}
	if(is.list(obj))
	{
		obj <- plyr::laply(obj, function(x) resToURI(api, x, type))
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
	if(!(type %in% api$resources)) stop(stringr::str_c("This API do not implement objects of type ",type,'. See ', deparse(substitute(api)),'$resources for more.'))
	schema <- stringr::str_c(api[[type]]$url,'schema')
	type_spec <- httr::content(httr::GET(schema))
	# Print a data.frame with the fields
	spec <- plyr::ldply(type_spec$fields, plyr::summarize, help = help_text, type = type, null = as.character(nullable), unique = unique, values = ifelse(exists('choices'), stringr::str_c(unlist(choices), collapse=', ') , ''))
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
