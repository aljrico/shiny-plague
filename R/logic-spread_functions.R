#' Calculates in-country spread chance
#' @param infectiousness number
#' @param infected_proportion proportion of infected population
#'
#'@export 
getInCountrySpreadChance <- function(infectiousness, infected_proportion){
  in_country_spread_factor <- 1
  result <- tanh(in_country_spread_factor * (1 - infected_proportion) + infectiousness)
  print(result)
  return(result)
}

#' Calculates cross-country spread chance
#' @param infectiousness number
#' @param infected_proportion proportion of infected population
#'
#'@export 
getCrossCountrySpreadChance <- function(infectiousness, infected_proportion){
  cross_country_spread_factor <- 5e-4
  tanh(infectiousness + sqrt(infected_proportion * cross_country_spread_factor))
}