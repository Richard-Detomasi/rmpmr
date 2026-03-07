#' persist
#'
#' @param colonies list with lists of modelled colonies
#'
#' @return value
#' @export

persist <- function(colonies) {
  infected <- colonies[[1]][,3]
  pops.mp <- rowSums(colonies[[1]][,1:4])
  for (i in 2:length(colonies)){
    infected <- rbind(infected, colonies[[i]][,3])
    pops.mp <- rbind(infected, rowSums(colonies[[i]][,1:4]))
  }
  infec.tot <- colSums(infected)
  tot.mp <- colSums(pops.mp)
  # plot(infec.tot, type="l")
  # lines(tot.mp, col = "blue")
  max(which((infec.tot / tot.mp) > 0 & is.finite(infec.tot / tot.mp) & is.nan(infec.tot / tot.mp) == FALSE))
}

