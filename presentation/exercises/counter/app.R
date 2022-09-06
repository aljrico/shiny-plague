source('presentation/exercises/counter/producerButton.R')

ReactiveClass <- R6::R6Class(
  "ReactiveClass",
  private = list(
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function(){
      private$reactiveDep(rnorm(1))
      invisible()
    }
  ),
  public = list(
    initialize = function(){
      private$reactiveDep <- function(x) NULL
    },
    reactive = function(){
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    }
  )
)

Counter <- R6::R6Class(
  "Counter",
  inherit = ReactiveClass,
  private = list(
    value = 0
  ),
  public = list(
    increaseValue = function(by = 1){
      private$value <- private$value + by
      private$invalidate()
    },
    decreaseValue = function(by){
      private$value <- private$value - by
      private$invalidate()
    },
    getValue = function(){
      private$value
    }
  )
)

Producer <- R6::R6Class(
  'Producer',
  public = list(
    initialize = function(cost, productivity){
      private$cost <- cost
      private$productivity <- productivity
    },
    getCost = function(){
      private$cost
    },
    getProductivity = function(){
      private$productivity
    },
    isAvailable = function(currentWealth){
      currentWealth >= self$getCost()
    }
  ),
  private = list(
    cost = numeric(0),
    productivity = numeric(0)
  )
)

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
    producer = Producer$new(cost = 10, productivity = 1),
    automaticProduction = automaticProduction
  )
  
  producerButton_server(
    id = 'producer2',
    counter = counter,
    producer = Producer$new(cost = 100, productivity = 5),
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
