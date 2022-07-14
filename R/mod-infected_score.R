infected_score_ui <- function(id = 'infected_score'){
  ns <- shiny::NS(id)
  
  tagList(
    uiOutput(ns('score'))
  )
}

infected_score_server <- function(id = 'infected_score', gameState){
  moduleServer(id, function(input, output, session){
    
    output$score <- renderUI({
      h3(gameState()$getScore())
    })
  })
}