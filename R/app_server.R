#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  gameState <- GameState$new()$reactive()
  core_loop(gameState)
  mod_map_server(gameState = gameState)
  infected_score_server(gameState = gameState)
  
}
