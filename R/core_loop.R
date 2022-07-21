core_loop <- function(gameState){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(10)
    observeEvent(loop(), {
      gameState()$progressInfection()
    })
  })
}