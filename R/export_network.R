#' @title Get aggregated informations on a network
#' 
#' @description Internal use
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param id the id of the network
#' @param level the level at which to aggregate data
#' @param ... reserved for future options
getElements <- function(api, id, level = 'taxa', ...)
{
	metadata <- NULL
	vertices_df <- NULL
	edges_list <- NULL
	if(!(level %in% c('taxa', 'population', 'item'))) stop("Level must be one of taxa, population, item")
	network <- getNetwork(api, id)
	edge_list <- adply(network$interactions, 1, function(x) unlist(getInteraction(api, x)))
	edge_list <- edge_list[,-1]
	if(level == 'taxa')
	{
		edge_list$from <- edge_list$taxa_from
		edge_list$to <- edge_list$taxa_to
		all_taxa <- unique(c(edge_list$to, edge_list$from))
		vertices_list <- alply(all_taxa, 1, function(x) getTaxa(api, x))
		vertices_df <- data.frame(laply(vertices_list, function(x) x))
		attrnames <- colnames(vertices_df)
		vertices_df <- vertices_df[,c('id', attrnames[!attrnames=='id'])]
	}
	return(list(vertices = vertices_df, edges = edge_list, metadata = metadata))
}

#' @title Export a network to igraph
#' @export
#' 
#' @description ...
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param id the id of the network
#' @param level the level of aggregation
#' @param ... reserved for future options
toIgraph <- function(api, id, level = 'taxa', ...)
{
	elements <- getElements(api, id, level, ...)
	edge_list <- elements$edges
	vertices_df <- elements$vertices
	edge_list <- edge_list[,c('from', 'to', 'taxa_from', 'taxa_to', 'link_type')]
	G <- graph.data.frame(edge_list, vertices=vertices_df)
	return(G)
}

#' @title Export a network to cheddar
#' @export
#' 
#' @description ...
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param id the id of the network
#' @param level the level of aggregation
#' @param ... reserved for future options
toCheddar <- function(api, id, level = 'taxa', ...)
{
	elements <- getElements(api, id, level, ...)
   stop("Coming soon")
}
