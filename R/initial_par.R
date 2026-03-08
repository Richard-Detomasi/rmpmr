#' initial_par
#'
#' @param pop matrix with 7 columns corresponding to suscepts, exp, infect, recov, time, newborns, deaths
#' @param dist squear matrix of conectivity among colonies
#' @param init_infect number of initial infect. Default 5
#' @param init_cols number of initial colonies Default 1
#' @param itera number of iterations. Default 1
#' @param ... All other param of rmpmr_seir
#'
#' @return list of lists
#' @export
#'
#' @examples
#' \dontrun{
#'  modelados <- initial_par(pop, dist_col_URY)
#' }

initial_par <- function(pop, dist, init_infect = 5, init_cols = 1, itera = 1, ...) {
  initial <- list(NA)
  if (init_cols == 1){
    inf_col <- rep(1:nrow(pop), ceiling(itera/nrow(pop)))
  } else {
    inf_col <- sample(1:nrow(pop), init_cols * itera, TRUE)
  }
  for (f in 1:length(inf_col)){
    pop[,3] <- 0
    pop[inf_col[f],3] <- init_infect
    pop[inf_col[f],1] <- pop[inf_col[f],1] - init_infect ## agregado para mantener el numero
    initial[[f]] <- persistence(pop, dist)
    print(f)
  }
  return(initial)
}