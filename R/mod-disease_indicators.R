disease_indicators_ui <- function(id = 'infected_score'){
  ns <- shiny::NS(id)
  return(
    div(
      style = "display: flex; flex-diretion: row; justify-content: space-around; align-items: center; width: 100%",
      diseaseIndicator(ns("lethality"), "Lethality",value = 0),
      diseaseIndicator(ns("infectiousness"), "Infectiousness",value = 0),
      diseaseIndicator(ns("visibility"), "Visibility",value = 0)
    )
  )
}

disease_indicators_server <- function(id = 'infected_score', gameState){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      updateDiseaseIndicator(session, id = ns("infectiousness"), value = 10 * gameState()$getInfectiousness(), palette = "infectiousness")
      updateDiseaseIndicator(session, id = ns("lethality"), value = 10 * gameState()$getLethality(), palette = "lethality")
      updateDiseaseIndicator(session, id = ns("visibility"), value = 10 * gameState()$getVisibility(), palette = "visibility")
    })
  })
}
