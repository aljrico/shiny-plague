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
        diseaseIndicator(ns("infectiousness"), "infectiousness",value = 10)
      ),
      column(
        width = 4,
        diseaseIndicator(ns("visibility"), "visibility",value = 5)
      )
    )
  )
}

disease_indicators_server <- function(id = 'infected_score', gameState){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    new_lethality <- reactive({
      invalidateLater(5000)
      sample(1:100, 1)
    })
    
    observeEvent(new_lethality(),{
      print(paste0("lethality indicator: ", new_lethality()))
      updateDiseaseIndicator(session, id = ns("lethality"), value = new_lethality())
    })
    
    new_infectiousness <- reactive({
      invalidateLater(5000)
      sample(1:100, 1)
    })
    
    observeEvent(new_infectiousness(),{
      updateDiseaseIndicator(session, id = ns("infectiousness"), value = new_infectiousness())
    })
    
    new_visibility <- reactive({
      invalidateLater(5000)
      sample(1:100, 1)
    })
    
    observeEvent(new_visibility(),{
      updateDiseaseIndicator(session, id = ns("visibility"), value = new_visibility())
    })
    
    
    
  })
}
