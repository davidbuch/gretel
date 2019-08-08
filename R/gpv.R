#' Generalized Path Value
#'
#' Calculates the generalized path value of a user-specified path through 
#' \code{sociomatrix}. Parameter \code{alpha} sets the alpha-norm used in calculation.
#' 
#' As a rule of thumb, alpha close to 0 will downweight the impact of particular
#' tie strengths and upweight the impact of binary path length. Alpha equal to 
#' infinity will recapitulate the traditional path value measure of Peay (1980)
#' and is therefore the default. In other words, the value of a path under 
#' \code{alpha = Inf} will be the value of the weakest tie. The value of the same
#' path under \code{alpha = 0} will be the inverse of its binary length.
#' 
#'
#' @param sociomatrix a nonnegative, real valued sociomatrix.
#' @param path an integer vector of node indices from \code{sociomatrix}.
#' @param alpha a nonnegative real number that sets the 'alpha-norm' parameter for 
#'     generalized path value calculation.
#' @param node_costs a list of costs, in order, of all nodes represented in the 
#'     sociomatrix, all are assumed 0 if unspecified
#'     
#' @seealso \code{\link{opt_gpv}} to identify the path of optimal 'gpv' between two nodes 
#'     and \code{\link{all_opt_gpv}} to identify the optimal paths between all pairs of 
#'     nodes. Calling \code{\link{generate_proximities}} with \code{mode = 'gpv'} 
#'     returns a matrix 'gpv' values for the optimal paths between all pairs of 
#'     nodes.
#' 
#' @examples
#' ## Calculate gpv along a path in a sociomatrix
#' gpv(YangKnoke01, path = c(1,2,5), alpha = 1)
#' 
#' ## The same calculation, with nonzero node costs
#' gpv(YangKnoke01, path = c(1,2,5), alpha = 1, node_costs = c(1,3,3,2,1))
#' 
#' ## This path doesn't exist
#' gpv(YangKnoke01, path = c(1,2,4,5), alpha = 0)
#' 
#' @export
gpv <- function(sociomatrix, path, alpha = Inf, node_costs = NULL){
  if(is.null(node_costs)){
    node_costs <- rep(0, nrow(sociomatrix))
  }
  intermediate_nodes <- path[-c(1,length(path))]
  if(alpha == 0){
    # effective distance matrix
    d_eff_mat <- 1/as_binary(sociomatrix)
    d_eff_path <- sum_path(d_eff_mat, path) + sum(node_costs[intermediate_nodes])
    path_value <- 1/d_eff_path
  } else if(alpha < Inf){
    d_eff_mat <- 1/(sociomatrix^alpha)
    d_eff_path <- sum_path(d_eff_mat, path) + sum(node_costs[intermediate_nodes])
    path_value <- (1/d_eff_path)^(1/alpha)
  } else if(alpha == Inf){
    path_value <- min_tie(sociomatrix, path)
  }
  return(path_value)
}


