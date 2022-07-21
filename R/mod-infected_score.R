infected_score_ui <- function(id = 'dna_score'){
  ns <- shiny::NS(id)
  
  div(
    style="display: flex; flex-direction: row; justify-content: center; align-items: center;",
    h3(
      style = "display: flex; flex-direction: row; justify-content: center; align-items: center;",
      icon('dna', style = 'margin-right: 2px;'),
     textOutput(ns('score'))
    )
  )
}

infected_score_server <- function(id = 'dna_score', gameState){
  moduleServer(id, function(input, output, session){
    
    output$score <- renderText({
      gameState()$getDNAPoints()
    })
  })
}