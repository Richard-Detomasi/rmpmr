## code to prepare `pop` dataset goes here
pop <- matrix(rep(c(suscept = 90, exp = 0, infect= 0, recov = 0, time = 0, newborns = 0, deaths = 0), 25), nrow = 25, ncol = 7, byrow = T)

usethis::use_data(pop, overwrite = TRUE)
