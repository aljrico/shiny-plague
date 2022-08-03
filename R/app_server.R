#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  gameState <- GameState$new()$reactive()
  mapManager <- MapManager$new()
  diseaseManager <- DiseaseManager$new(mapManager$getMapData())
  mapManager$updateMapData(diseaseManager$sproutFirstInfected(mapManager$getMapData()))
  core_loop(gameState,mapManager,diseaseManager)
  mod_map_server(gameState = gameState, mapManager = mapManager)
  infected_score_server(gameState = gameState)
  
}
