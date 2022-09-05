AbilityCard <- R6::R6Class(
  "AbilityCard",
  inherit = Card,
  public = list(
    setState = function(new_state = c("available", "cant_afford","unavailable", "purchased")){
      if('purchased' %in% new_state){
        private$cost <- private$cost * 2
      }else {
        private$state <-  new_state
      }
    }
  )
)
