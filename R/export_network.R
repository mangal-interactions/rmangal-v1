#' @title Format a trait or environment
#' @param rec the result from a getTrait or getEnvironment query
formatTraitEnv <- function(rec)
{
	name <- stringr::str_c(strsplit(rec$name, ' ')[[1]],collapse='_')
	rec <- rec[!names(rec) %in% c('name','owner')]
	names(rec) <- stringr::str_c(name, '__', names(rec))
	return(rec)
}

#' @title Transforms a taxa to a vector
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id a taxa id
taxaToVector <- function(api, id)
{
	taxa <- getTaxa(api, id)
	tav <- unlist(taxa[!names(taxa)=='traits'])
	if(length(taxa$traits) > 0)
	{
		traits <- plyr::llply(taxa$traits, function(x) unlist(formatTraitEnv(getTrait(api, x))))
		tav <- c(tav, unlist(traits))
	}
	return(tav)
}

#' @title Make a table with taxa level infos
#'
#' @param api a \code{\link{mangalapi}} object
#' @param id a vector of taxa id
makeTaxaTable <- function(api, id)
{
	taxa <- plyr::alply(id, 1, function(x) taxaToVector(api, x))
	all_columns <- unique(unlist(plyr::llply(taxa, names)))
	taxa_table <- plyr::ldply(taxa, function(x) x[all_columns])[,-1]
	colnames(taxa_table) <- all_columns
   id_pos <- which(colnames(taxa_table)=='id')
   if(id_pos != 1)
   {
      n_1 <- taxa_table[,1]
      taxa_table[,1] <- taxa_table[,id_pos]
      taxa_table[,id_pos] <- n_1
      colnames(taxa_table)[c(1, id_pos)] <- colnames(taxa_table)[c(id_pos, 1)]
   }
	return(taxa_table)
}

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
	Mdf <- NULL
	Vdf <- NULL
	Edf <- NULL
	if(!(level %in% c('taxa', 'population', 'item'))) stop("Level must be one of taxa, population, item")
	network <- getNetwork(api, id)
	Edf <- plyr::adply(network$interactions, 1, function(x) unlist(getInteraction(api, x)))
	Edf <- Edf[,-1]
	if(level == 'taxa')
	{
		Edf$from <- Edf$taxa_from
		Edf$to <- Edf$taxa_to
		all_taxa <- unique(c(Edf$to, Edf$from))
		V_list <- plyr::alply(all_taxa, 1, function(x) getTaxa(api, x))
		V_list <- plyr::laply(V_list, function(x) x$id)
		Vdf <- makeTaxaTable(api, V_list)
	}
	return(list(vertices = Vdf, edges = Edf, metadata = Mdf))
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
# TODO PERFORMANCE ISSUE - get taxa information AT THE END!
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
