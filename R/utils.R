#' Birth pulse functions
#'
#' @param t time in years
#' @param s synchrony
#' @param omega offset
#' @param k scale of function
#'
#' @return vector

birth <- function(t, s, omega, k){
  k * exp(-s * cos(pi * t - omega )^2)  ## uses time in years and maximum is 1
}


#' Reproductivity success over time
#'
#' @param t time in days
#' @param s synchrony
#' @param omega offset
#' @param glob.rep total reproductive rate
#' @return vector
#' @export

r_t <- function(t, s = 0.4, omega = 4.55, glob.rep = 0.102/365.25){
  birth(t/365.25, s, omega, k= glob.rep/ integrate(function(x) birth(x, s, omega, 1), lower = 0, upper = 1)$value)
}


#' Connectivity function
#'
#' @param dist pairwise distance
#' @param d1 sigmoid slope
#' @param d2 distance threshold
#'
#' @return vector
#' @export

conec.dist <- function(dist, d1=0.0005, d2=10000) {
  1 - (1 / (1+exp(-d1 * (dist - d2))))
}

