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
      self$reactive()
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