#' Lists the populations of a particular taxa
#'
#' Makes a list of all populations associated to a particular taxa.
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param taxa A \code{taxa} object in \code{list} format
listPop <- function(API, taxa)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  checkArg(taxa)
  list_pop <- httr::GET(paste(API$url,'taxa',taxa$id,'pop',sep='/'))
  if(list_pop$status_code == 200) return(content(list_pop))
}