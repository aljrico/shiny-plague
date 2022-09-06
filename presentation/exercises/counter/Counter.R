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