#' Lists the states of a particular population
#'
#' Makes a list of all states associated to a particular population.
#'
#' @export
#'
#' @param API An object of class \code{\link{mangalAPI}}
#' @param taxa A \code{taxa} object in \code{list} format
#' @param pop A \code{population} object in \code{list} format
listPopstate <- function(API, taxa, pop)
{
  if(class(API)!="mangal") stop("The API argument must be a valid mangalAPI object")
  a_ply(c(taxa, pop), 1, checkArg)
  request <- paste(API$url, 'taxa', pop$taxa, 'pop', pop$id, 'state', sep='/')
  if(is.null(API$auth))
  {
    warning("You are not authenticated, only public datasets are listed")
    list_pop <- httr::GET(request)
  } else {
    list_pop <- httr::GET(request, API$auth)
  }
  if(list_pop$status_code == 200) return(content(list_pop))
}
