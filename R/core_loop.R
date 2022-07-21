core_loop <- function(gameState){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(100)
    observeEvent(loop(), {
      gameState()$addTick()
      gameState()$changeScore(1)
      gameState()$progressInfection()
    })
  })
}