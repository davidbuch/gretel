as_binary <- function(sociomatrix){
  return(matrix(as.numeric(as.logical(sociomatrix)),nrow = nrow(sociomatrix)))
}

sum_path <- function(edge_data, path){
  path_sum <- 0
  nsteps <- length(path) - 1
  for(i in 1:nsteps){
    on <- path[i]
    to <- path[i + 1]
    path_sum <- path_sum + edge_data[on, to]
  }
  return(path_sum)
}

#' Unpacks a Path from a Dijkstra-Format Spanning Tree
#'
#' Used with \code{all_opt_gpv} and \code{all_opt_ppv} to 
#' unpack individual paths from the Dijkstra-format trees that 
#' those functions return.
#' 
#' Returns \code{NA} if a path does not exist
#' 
#' @param tree a Dijkstra-format tree returned by \code{all_opt_gpv} or \code{all_opt_ppv}
#' @param source an integer index corresponding to a node in \code{sociomatrix}
#' @param target an integer index corresponding to a node in \code{sociomatrix}
#' 
#' @example
#' # Identify the optimal paths
#' best_paths <- all_opt_gpv(YangKnoke01, p = 1)
#' 
#' # 'best_paths' will contain a list of trees in dijkstra's format.
#' # 'best_paths[[i]]' is the tree encoding shortest paths from source
#' #     node 'i' to all alters. We can return the optimal path from 
#' #     node 1 to node 4 as follows.
#' unpack(best_paths[[1]], source = 1, target = 4)
#' 
#' @export
unpack<- function(tree, source, target){
  # maybe just accept unpacking the path backwards to save memory reallocations
  if(is.na(tree[target])){
    path <- NA
  }else{
    on <- target
    path <- c(on)
    while(on != source){
      prev <- tree[on]
      path <- c(prev,path)
      on <- prev
    }
  }
  return(path) 
}


min_tie <- function(sociomatrix, path){
  nties <- length(path) - 1
  tie_vals <- c()
  for(i in 1:nties){
    on <- path[i]
    to <- path[i + 1]
    tie_vals <- c(tie_vals, sociomatrix[on, to])
  }
  return(min(tie_vals))
}

scale_to_odds <- function(sociomatrix, odds_scale, odds_scale_by_node){
  if(is.null(odds_scale_by_node)){
    sociomatrix <- sociomatrix/odds_scale
  }else{
    nv <- nrow(sociomatrix)
    for(i in 1:nv){
      sociomatrix[i,] <- sociomatrix[i,]/odds_scale_by_node[i]
    }
  }
  return(sociomatrix)
}

shortest_path <- function(distance_matrix, source, target, node_costs, p_finite = T){
  if(p_finite == T){
    prev <- dijkstra_nodes(distance_matrix, source, node_costs)
  } else {
    prev <- dijkstra_inf(distance_matrix, source)
  }
  if(is.na(prev[target])){
    path <- NA # path is disconnected
  } else {
    # Now walk backwards through 'prev'
    on <- target
    path <- c(on)
    while(on != source){
      on <- prev[on]
      path <- c(on, path)
    }
  }
  return(path)
}

APSP <- function(dist, node_costs, p_finite = T){
  nv <- nrow(dist)
  paths <- list()
  if(p_finite == T){
    for(s in 1:nv){
      paths[[s]] <- dijkstra_nodes(dist = dist, src = s, node_costs = node_costs)
    }
  }else{
    for(s in 1:nv){
      paths[[s]] <- dijkstra_inf(dist = dist, src = s)
    }
  }
  return(paths)
}

sconduct <- function(sociomatrix){
  nv <- nrow(sociomatrix)
  for(i in 1:nv){sociomatrix[i,i] <- 0}
  laplacian <- diag(apply(sociomatrix,MARGIN = 1,sum)) - sociomatrix
  sconductivity <- 1/ResistorArray::Wu(laplacian)
  return(sconductivity)
}