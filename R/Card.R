Card <- R6::R6Class(
  "Card",
  private = list(
    category = NULL,
    cost = NULL,
    lethality = NULL,
    infectiousness = NULL,
    visibility = NULL,
    state = NULL
  ),
  public = list(
    id = NULL,
    initialize = function(category, cost, lethality, infectiousness, visibility, state = c("unavailable", "available", "cant_afford")) {
      private$category  <- category 
      private$cost  <- cost 
      private$lethality  <- lethality 
      private$infectiousness  <- infectiousness 
      private$visibility <- visibility
      private$state <- 'available'
      self$id <- digest::digest(self, algo = 'murmur32')
    },
    setState = function(new_state = c("available", "cant_afford","unavailable")){
      private$state <-  match.arg(new_state)
    },
    getCategory = function(){
      private$category
    },
    getState = function(){
      private$state
    },
    getLethalityImpact = function(){
      private$lethality
    },
    getInfectiousnessImpact = function(){
      private$infectiousness
    },
    getVisibilityImpact = function(){
      private$visibility
    },
    getCost = function(){
      private$cost
    },
    getCard = function(){
      list(
        category = private$category,
        cost = private$cost,
        lethality = private$lethality,
        infectiousness = private$infectiousness,
        visibility = private$visibility,
        state = private$state
      )
    },
    isAvailable = function(){
      self$getState() != 'unavailable'
    },
    print = function(){
      cat("Card: \n")
      cat(" id: ", self$id, "\n", sep = "")
      cat(" category: ", private$category, "\n", sep = "")
      cat(" cost: ", private$cost, "\n", sep = "" )
      cat(" lethality: ", private$lethality, "\n", sep = "" )
      cat(" infectiousness: ", private$infectiousness, "\n", sep = "")
      cat(" visibility: ", private$visibility, "\n", sep = "" )
      cat(" state: ", private$state, "\n", sep = "" )
      invisible(self)
    }
  )
)