current_directory <- this.path::this.path()
setwd(dirname(current_directory))
source('producerButton.R')
source('ReactiveClass.R')
source('Counter.R')
source('Producer.R')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shiny::textOutput('counter'),
  shiny::actionButton(
    inputId = 'button',
    label = 'Press me!'
  ),
  producerButton_ui(id = 'producer1', label = 'Producer 1'),
  producerButton_ui(id = 'producer2', label = 'Producer 2')
)

server <- function(input, output, session){
  
  counter <- Counter$new()$reactive()
  loop <- shiny::reactiveTimer(1000)
  automaticProduction <- shiny::reactiveVal(0)
  
  producerButton_server(
    id = 'producer1',
    counter = counter,
    producer = Producer$new(cost = 10, productivity = 1)$reactive(),
    automaticProduction = automaticProduction
  )
  
  producerButton_server(
    id = 'producer2',
    counter = counter,
    producer = Producer$new(cost = 100, productivity = 5)$reactive(),
    automaticProduction = automaticProduction
  )
  
  observeEvent(loop(), {
    counter()$increaseValue(automaticProduction())
  })
  
  output$counter <- renderText({
    counter()$getValue()
  })
  
  observeEvent(input$button, {
    counter()$increaseValue()
  })
}

shiny::shinyApp(ui, server)
