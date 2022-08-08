disease_indicators_ui <- function(id = 'infected_score'){
  ns <- shiny::NS(id)
  
  tagList(
    uiOutput(ns('score')),
    fluidRow(
      column(
        width = 4,
        diseaseIndicator(ns("lethality"), "lethality",value = 0)
      ),
      column(
        width = 4,
        diseaseIndicator(ns("infectiousness"), "infectiousness",value = 0)
      ),
      column(
        width = 4,
        diseaseIndicator(ns("visibility"), "visibility",value = 0)
      )
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
