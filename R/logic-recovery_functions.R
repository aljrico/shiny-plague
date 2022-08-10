#' Calculates in-country spread chance
#' @param medical_progress gameState parameter
#'
#'@export 
getRecoveryChance <- function(medical_progress){
  recovery_factor <- 0.1
  tanh(medical_progress * recovery_factor)
}