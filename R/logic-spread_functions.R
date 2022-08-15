#' Calculates in-country spread chance
#' @param infectiousness number
#' @param infected_proportion proportion of infected population
#'
#'@export 
getInCountrySpreadChance <- function(infectiousness, infected_proportion){
  in_country_spread_factor <- 0.1
  result <- tanh(in_country_spread_factor * (1 + infectiousness) ^ 2 * (1 - infected_proportion) ^ 2)
  return(result)
}

#' Calculates cross-country spread chance
#' @param infectiousness number
#' @param infected_proportion proportion of infected population
#'
#'@export 
getCrossCountrySpreadChance <- function(infectiousness, infected_proportion){
  cross_country_spread_factor <- 1e-3
  tanh(cross_country_spread_factor * (1 + infectiousness) * (1 + infected_proportion))
}

#' Calculates airborne spread chance
#' @param infectiousness number
#' @param infected_proportion proportion of infected population
#'
#'@export 
getAirborneSpreadChance <- function(infectiousness, airborne_bonus = 0){
  airborne_spread_factor <- 1e-3
  airborne_factor <- (1 + airborne_bonus) ^ 7
  infectiousness_factor <- 1 + infectiousness
  tanh(airborne_spread_factor * airborne_factor * infectiousness_factor)
}