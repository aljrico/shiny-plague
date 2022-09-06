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
    cost = 10,
    productivity = 1
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
  producer <- Producer$new()
  loop <- shiny::reactiveTimer(1000)
  isAutomaticProductionActive <- shiny::reactiveVal(FALSE)
  
  observeEvent(loop(), {
    if(isAutomaticProductionActive()) counter()$increaseValue()
  })
  
  output$counter <- renderText({
    counter()$getValue()
  })
  
  observeEvent(input$button, {
    counter()$increaseValue()
  })
  
  observe({
    if(producer$isAvailable(counter()$getValue())){
      shinyjs::enable('producer_button')
    }else{
      shinyjs::disable('producer_button')
    }
  })
  
  observeEvent(input$producer_button, {
    isAutomaticProductionActive(TRUE)
    counter()$decreaseValue(producer$getCost())
  })
}

shiny::shinyApp(ui, server)
