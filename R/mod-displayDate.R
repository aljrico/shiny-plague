displayDate_ui <- function(id = 'displayDate'){
  ns <- NS(id)
  div(
    style = "position: absolute; top: 12%; right: 6%;",
    textOutput(ns("date"))
  )
}

displayDate_server <- function(id = 'displayDate', gameState){
  moduleServer(id, function(input, output, session){
    output$date <- renderText({
      format(as.Date(gameState()$getDate()), "%Y-%m-%d")
      
    })
  })
}