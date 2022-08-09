core_loop <- function(gameState){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(5000)
    observeEvent(loop(), {
      gameState()$increaseDate()
      gameState()$progressInfection()
    })
  })
}