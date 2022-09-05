#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  gameState <- GameState$new()$reactive()
  core_loop(gameState, end_game_condition)
  mod_map_server(gameState = gameState)
  displayDate_server(gameState = gameState)
  
  # End game condition
  end_game_condition <- reactive({
    gameState()$getTotalInfected() == 0
  })
  
  
  observeEvent(end_game_condition(), {
    if(!end_game_condition()) return(NULL)
    shinyWidgets::sendSweetAlert(
      title = "Game Finished",
      text = glue::glue("Congratulations! You managed to kill {formatNumbers(gameState()$getTotalDeaths())} people! I hope you are happy"),
      showCloseButton = FALSE,
      closeOnClickOutside = FALSE,
      type = 'success'
    )
  })
}
