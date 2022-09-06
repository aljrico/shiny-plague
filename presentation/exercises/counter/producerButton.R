producerButton_ui <- function(id){
  ns <- shiny::NS(id)  
  actionButton(
    inputId = ns('producer'),
    label = 'Producer'
  )
}

producerButton_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    observeEvent(input$producer, {
      print(input$producer)
    })
  })
}