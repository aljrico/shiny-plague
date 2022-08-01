#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  gameState <- GameState$new()$reactive()
  cardStack <- CardStack$new(20) # number creates number of cards available in the shop
  core_loop(gameState)
  mod_map_server(gameState = gameState, cardStack = cardStack)
  infected_score_server(gameState = gameState)
}
