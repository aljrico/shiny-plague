Producer <- R6::R6Class(
  'Producer',
  inherit = ReactiveClass,
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
    },
    increaseCost = function(){
      private$cost <- round(private$cost * 1.25)
      private$invalidate()
    }
  ),
  private = list(
    cost = numeric(0),
    productivity = numeric(0)
  )
)