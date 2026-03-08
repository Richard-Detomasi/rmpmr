#' Dinamic model
#'
#' @param pop matrix with 7 columns corresponding to suscepts, exp, infect, recov, time, newborns, deaths
#' @param dist squear matrix of conectivity among colonies
#' @param dt Time step in days. Default 1
#' @param ntt Max. limit of times in days. Default 18250 (aprox. 50 years)
#' @param glob.rep Total reproductive rate (1/days). Default  0.102/365.25
#' @param mu Natural mortality rate (1/days). Default glob.rep
#' @param omega Offset. Default 4.55 
#' @param s Synchrony. Default 0.4
#' @param epsilon Immunity waining rate (1/days). Default 1/135
#' @param delta Proportion of exposued to devlop infection. Default 0.1
#' @param tau Exit rate from exposed class. Default 1/21
#' @param alpha Disease-induced mortality rate. Default 1/11
#' @param beta probability of transmission given contact. Default 0.9322064 
#' @param dens If T by density dependent transmission, else by frequency dependent transmission. Default T
#' @import stats
#' @return list of lists
#' @export
#' @examples
#' \dontrun{
#'  rmpmr_seir(pop, dist_col_URY)
#' }
 
rmpmr_seir <- function(pop, dist, dt = 1, ntt = 18250, glob.rep = 0.102/365.25, 
                       mu = glob.rep, omega = 4.55, s = 0.4, epsilon = 1/135,
                       delta = 0.1, tau = 1/21, alpha = 1/11, beta = 0.9322064,
                       dens = T) {
  colonies <- lapply(split(t(pop), rep(1:nrow(pop), each = ncol(pop))),
                     FUN = matrix, nrow = 1, ncol = 7, byrow = T)
  infected <- matrix(NA, nrow = nrow(pop), ncol = 1)
  # t+dt
  for (i in 1:ntt) {
    for(k in 1:nrow(pop)){
      infected[k,1] <- colonies[[k]][i,3]
    }
    infect_weight <- dist %*% infected
    for (j in 1:nrow(pop)) {
      newborns <- c(suscepts = rbinom(1, colonies[[j]][i,1], integrate(r_t, colonies[[j]][i,5], colonies[[j]][i,5] + dt)$value),
                    recup = rbinom(1, sum(colonies[[j]][i,2:4]), integrate(r_t, colonies[[j]][i,5], colonies[[j]][i,5] + dt)$value))
      recup_to_suscept <- sum(rexp(colonies[[j]][i,4], epsilon) < dt)
      suscept_to_exp <- ifelse(sum(colonies[[j]][i,1:4], na.rm = T) > 0,
                               ifelse(dens == T, rbinom(1, colonies[[j]][i,1], infect_weight[j] * beta / sum(colonies[[j]][i,1:4], na.rm = T)),
                                      rbinom(1, colonies[[j]][i,1], infect_weight[j] * beta)),
                               0)
      exp_out <- sum(rexp(colonies[[j]][i,2], tau) < dt)
      exp_to_infect <- rbinom(1, exp_out, delta)
      exp_to_recov <-  exp_out - exp_to_infect
      dis_deaths <- sum(rexp(colonies[[j]][i,3], alpha) < dt)
      deaths <- c(suscepts = ifelse((colonies[[j]][i,1] - suscept_to_exp) > 0, sum(rexp(colonies[[j]][i,1] - suscept_to_exp, mu) < dt), 0),
                  exp = ifelse((colonies[[j]][i,2] - exp_to_recov - exp_to_infect) > 0, sum(rexp(colonies[[j]][i,2] - exp_to_recov - exp_to_infect, mu) < dt), 0),
                  infect = ifelse((colonies[[j]][i,3] - dis_deaths) > 0, sum(rexp(colonies[[j]][i,3] - dis_deaths, mu) < dt), 0),
                  recup = ifelse((colonies[[j]][i,4] - recup_to_suscept) > 0, sum(rexp(colonies[[j]][i,4] - recup_to_suscept, mu) < dt), 0))
      colony1_t1 <- matrix(c(colonies[[j]][i,1] + newborns[1] + recup_to_suscept - deaths[1] - suscept_to_exp,
                             colonies[[j]][i,2] + suscept_to_exp - deaths[2] - exp_to_recov - exp_to_infect,
                             colonies[[j]][i,3] + exp_to_infect - deaths[3] - dis_deaths,
                             colonies[[j]][i,4] + newborns[2] + exp_to_recov - deaths[4] - recup_to_suscept,
                             colonies[[j]][i,5] + dt,
                             sum(newborns),
                             sum(deaths) + dis_deaths), ncol = 7, byrow = T)
      colony1_t1[is.na(colony1_t1)] <- 0
      colonies[[j]] <- rbind(colonies[[j]], colony1_t1)
    }
  }
  return(colonies)
}
