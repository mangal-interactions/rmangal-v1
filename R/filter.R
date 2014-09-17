#' @title Construct a filtering query
#' @export
#' 
#' @description Returns a string formatted as a filtering query
#' 
#' @details
#' 
#' This functions takes a list and returns a formatted filtering query. It is used internally by \code{mangalSearch}. The next paragraphs detail the role of each element of the \code{filter} list.
#' 
#' \code{field} is the name of the field on which to search. A list of fields is available with \code{whatIs}.
#' 
#' \code{relation} is a string, giving the type of search to perform. It can takes the values \code{startswith}, \code{endswith}, \code{contains}, \code{exact}, \code{in} (check if \code{target} is one of the values of for multi-values fields), \code{gte}, \code{lte}, \code{gt}, \code{lt}, and finally \code{range}.
#' 
#' \code{target} is the value to search on. If \code{relation} is \code{range}, it must be given as a vector of two values
#' 
#' @param api the api to connect to
#' @param type the type of resource to filter
#' @param filter a list with objects field, relation, target
makefilter <- function(api, type, filter)
{
   f_filt <- list()
   if(!prod(names(filter) %in% c('field', 'relation', 'target'))) stop("The filter list must have names field, relation, target")
   if(!filter$relation %in% c('contains', 'startswith', 'endswith', 'in', 'exact', 'gt', 'gte', 'lt', 'lte', 'range')) stop(stringr::str_c("This relation (",filter$relation,") do not exist"))
   if(!(filter$field %in% whatIs(api, type)$field)) stop(stringr::str_c("Object of type ", type," have no field ", filter$field))
   if(filter$relation == 'range')
   {
   	if(length(filter$target) != 2) stop("The target should be a vector of two elements when searching a range")
   	filter$target <- stringr::str_c(filter$target,collapse=',')
   }
   f_filt[[stringr::str_c(filter$field,'__',filter$relation)]] <- filter$target
   return(f_filt)
}