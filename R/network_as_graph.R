#' @title Get a network
#' @export
#' 
#' @description ...
#' 
#' @param api a \code{\link{mangalapi}} object
#' @param id the id of the network
#' @param level the level of aggregation
#' @param ... reserved for future options
network_as_graph <- function(api, id, level = 'taxa', ...)
{
	if(!(level %in% c('taxa', 'population', 'item'))) stop("Level must be one of taxa, population, item")
	network <- getNetwork(api, id)
	edge_list <- adply(network$interactions, 1, function(x) unlist(getInteraction(api, x)))
	if(level == 'taxa')
	{
		edge_list$from <- edge_list$taxa_from
		edge_list$to <- edge_list$taxa_to
		all_taxa <- unique(c(edge_list$to, edge_list$from))
		vertices_df <- adply(all_taxa, 1, function(x) unlist(getTaxa(api, x)))
		vertices_df$X1 <- vertices_df$id
	}	
	edge_list <- edge_list[,-1]
	edge_list <- edge_list[,c('from', 'to', 'taxa_from', 'taxa_to', 'ecotype')]
	G <- graph.data.frame(edge_list, vertices=vertices_df)
	return(G)
}
