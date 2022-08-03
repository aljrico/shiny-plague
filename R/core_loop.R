core_loop <- function(gameState,mapManager,diseaseManager){
  moduleServer('core_loop', function(input, output, session){
    loop <- reactiveTimer(10000)
    observeEvent(loop(), {
      # progressInfection
      new_map_data <- diseaseManager$progressInfection(mapManager$getMapData(), gameState)
      mapManager$updateMapData(new_map_data)
    })
  })
}