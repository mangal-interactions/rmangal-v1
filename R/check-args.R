#' Check arguments passed to API functions
#'
#' Throws error messages if the arguments are not in the correct format
#'
#' @param obj The list object to check
checkArg <- function(obj)
{
  if(!is.list(obj)) stop("Objects must be passed as lists")
  if(is.null(obj$id)) stop("The object must have a ID attribute")
  if(!is.character(obj$id)) stop("The id attribute must be a character")
}