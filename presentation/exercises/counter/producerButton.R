producerButton_ui <- function(id, label){
  ns <- shiny::NS(id)  
  actionButton(
    inputId = ns('button'),
    label = label
  )
}

producerButton_server <- function(id, counter, producer, automaticProduction){
  shiny::moduleServer(id, function(input, output, session){
    
    observe({
      shiny::updateActionButton(
        inputId = 'button',
        label = paste0('Cost: ', producer()$getCost(), ' Production: ', producer()$getProductivity())
      )
    })
    
    observe({
      if(producer()$isAvailable(counter()$getValue())){
        shinyjs::enable('button')
      }else{
        shinyjs::disable('button')
      }
    })
    
    observeEvent(input$button, {
      automaticProduction(automaticProduction() + producer()$getProductivity())
      counter()$decreaseValue(producer()$getCost())
      producer()$increaseCost()
    })
  })
}