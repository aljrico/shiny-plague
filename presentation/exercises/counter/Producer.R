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