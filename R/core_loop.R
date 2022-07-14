core_loop <- function(gameState){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(1000)
    observeEvent(loop(), {
      gameState()$changeScore(1)
      gameState()$spreadInfection()
    })
  })
}