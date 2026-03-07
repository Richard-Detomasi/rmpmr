#' Population matrix
#'
#' A dataset containing the initialpopulation matrix for rmpmr_seir function
#' @family data
#' @format A matrix with 25 rows and 7 columns:
#' \describe{
#'   \item{suscept}{number of population susceptible at time 0}
#'   \item{exp}{number of population expoussed at time 0}
#'   \item{infect}{number of population infected at time 0}
#'   \item{recov}{number of population recovered at time 0}
#'   \item{time}{inicial time)}
#'   \item{newborns}{number of population born at time 0}
#'   \item{deaths}{number of population death at time 0}
#' }
"pop"


#' Connetivity matrix
#'
#' A dataset containing the initial connectivity matrix for rmpmr_seir function
#' based on uruguayan dataset of colonies (Botto 2021)
#' @family data
#' @format A matrix with 25 rows and 25 columns:
"dist_col_URY"