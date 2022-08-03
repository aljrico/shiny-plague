core_loop <- function(gameState,mapManager,diseaseManager){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(100)
    observeEvent(loop(), {
      gameState()$addTick()
      gameState()$changeScore(1)
      new_map_data <- diseaseManager$progressInfection(mapManager$getMapData())
      mapManager$updateMapData(new_map_data)
    })
  })
}