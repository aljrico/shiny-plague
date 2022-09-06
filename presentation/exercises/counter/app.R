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
  shiny::actionButton(
    inputId = 'producer_button',
    label = 'Activate Automatic Production'
  )
)

server <- function(input, output, session){
  
  counter <- Counter$new()$reactive()
  producer1 <- Producer$new(cost = 10, productivity = 100)
  producer2 <- Producer$new(cost = 1000, productivity = 5)
  loop <- shiny::reactiveTimer(1000)
  automaticProduction <- shiny::reactiveVal(0)
  
  observeEvent(loop(), {
    counter()$increaseValue(automaticProduction())
  })
  
  output$counter <- renderText({
    counter()$getValue()
  })
  
  observeEvent(input$button, {
    counter()$increaseValue()
  })
  
  observe({
    if(producer1$isAvailable(counter()$getValue())){
      shinyjs::enable('producer_button')
    }else{
      shinyjs::disable('producer_button')
    }
  })
  
  observeEvent(input$producer_button, {
    automaticProduction(automaticProduction() + producer1$getProductivity())
    counter()$decreaseValue(producer1$getCost())
  })
}

shiny::shinyApp(ui, server)
