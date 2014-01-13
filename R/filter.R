#' @title Construct a filtering query
#' @export
#' 
#' @description Returns a string formatted as a filtering query
#' 
#' @param api the api to connect to
#' @param type the type of resource to filter
#' @param filter a list with objects field, relation, target
makefilter <- function(api, type, filter)
{
   if(!prod(names(filter) %in% c('field', 'relation', 'target'))) stop("The filter list must have names field, relation, target")
   if(!filter$relation %in% c('contains', 'startswith', 'endswith', 'in', 'exact', 'gt', 'gte', 'lt', 'lte', 'range')) stop(str_c("This relation (",filter$relation,") do not exist"))
   if(!(filter$field %in% whatIs(api, type)$field)) stop(str_c("Object of type ", type," have no field ", type$field))
   return(str_c(filter$field,'__',filter$relation,'=',filter$target))
}
