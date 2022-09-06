producerButton_ui <- function(id, label){
  ns <- shiny::NS(id)  
  actionButton(
    inputId = ns('producer'),
    label = label
  )
}

producerButton_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    observeEvent(input$producer, {
      print(input$producer)
    })
  })
}