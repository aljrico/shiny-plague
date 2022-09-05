core_loop <- function(gameState, end_game_condition){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(500)
    observeEvent(loop(), {
      if(end_game_condition()) return(NULL)
      gameState()$increaseDate()
      gameState()$increaseMedicalProgress()
      gameState()$progressInfection()
    })
  })
}