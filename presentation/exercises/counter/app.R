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
      private$value <- private$value + 1
      private$invalidate()
    },
    getValue = function(){
      private$value
    }
  )
)

ui <- fluidPage(
  shiny::textOutput('counter'),
  shiny::actionButton(
    inputId = 'button',
    label = 'Press me!'
  )
)

server <- function(input, output, session){
  
  counter <- Counter$new()$reactive()
  output$counter <- renderText({
    counter()$getValue()
  })
  
  observeEvent(input$button, {
    counter()$increaseValue()
  })
}

shiny::shinyApp(ui, server)
