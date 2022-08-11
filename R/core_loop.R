core_loop <- function(gameState){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(2000)
    observeEvent(loop(), {
      gameState()$increaseDate()
      gameState()$increaseMedicalProgress()
      gameState()$progressInfection()
    })
  })
}