#' Calculates death chance
#' @param infectiousness number
#' @param infected_proportion proportion of infected population
#'
#'@export 
getDeathChance <- function(lethality){
  death_factor <- 1e-3
  tanh(death_factor * lethality)
}
