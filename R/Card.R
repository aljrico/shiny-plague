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
    initialize = function(category, cost, lethality, infectiousness, visibility, state = c("unavailable", "available", "cant_afford")) {
      cli::cli_alert('creating card')
      private$category  <- category 
      private$cost  <- cost 
      private$lethality  <- lethality 
      private$infectiousness  <- infectiousness 
      private$visibility <- visibility
      private$state <- match.arg(state)
    },
    setState = function(new_state = c("available", "cant_afford","unavailable")){
      private$state <-  match.arg(new_state)
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
    print = function(){
      cat("Card: \n")
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

CardStack <- R6::R6Class(
  "CardStack",
  private = list(
    shop_card_stack = NULL,
    init_card_stack = function(n_cards){

      cards <- data.frame(
        category = rep(c("Sneezing", "Coughing", "Pooping"), length.out = n_cards),
        cost = sample(1:3, n_cards, replace = TRUE),
        lethality = sample(-20:20, n_cards),
        infectiousness = sample(-20:20, n_cards),
        visibility = sample(-20:20, n_cards),
        state = rep(c("unavailable", "available", "cant_afford"), length.out = n_cards)
      )
      
      card_list <- do.call(Map, c(f= Card$new, cards)) 
      names(card_list) <- NULL
      return(card_list)
    }
  ),
  public = list(
    initialize = function(n_cards = 10) {
      cli::cli_h2('creating card stack')
      private$shop_card_stack <- private$init_card_stack(n_cards)
    },
    getCardStack = function(){
      order_cards <- function(cards){
        cards[order(sapply(cards, function(x) x$getCost()))]
      }
      private$shop_card_stack |> 
        order_cards()
    }
  )
)