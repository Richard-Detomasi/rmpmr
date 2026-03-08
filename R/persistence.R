#' persistence
#'
#' @param pop matrix with 7 columns corresponding to suscepts, exp, infect, recov, time, newborns, deaths
#' @param dist squear matrix of conectivity among colonies
#' @param threshold each distance threshold to eval. Default seq(0, 1000000, 100000)
#' @param iter number of iterations. Default 11
#' @param ... All other param of rmpmr_seir
#'
#' @return list of lists
#' @export
#'
#' @examples
#' \dontrun{
#'  persistence(pop, dist_col_URY)
#' }

persistence <- function(pop, 
                        dist, 
                        threshold = seq(0, 1000000, 100000), iter = 11, # Grano grueso / mayor recorrido para el umbral
                        ...) { 
  colonies <- list(NA)
  persisting <- NA
  for (g in 1:iter){
    spat.w <- conec.dist(dist, d2 = threshold[g])
    diag(spat.w) <- 1
    colonies[[g]] <- rmpmr_seir(pop, spat.w)
    persisting[g] <-persist(colonies[[g]])
  }
  result <- list(colonies, persisting)
  return(result)
}