CardsManager <- R6::R6Class(
  "CardsManager",
  private = list(
    shop_card_stack = NULL,
    init_card_stack = function(n_cards){
      list(
        Card$new(
          category = 'Sneezing',
          cost = 1,
          lethality = 0,
          infectiousness = 1,
          visibility = 1
        ),
        Card$new(
          category = 'Coughing',
          cost = 3,
          lethality = 1,
          infectiousness = 2,
          visibility = 1
        ),
        Card$new(
          category = 'Nausea',
          cost = 2,
          lethality = 0,
          infectiousness = 0.5,
          visibility = 1.5
        ),
        Card$new(
          category = 'Rash',
          cost = 3,
          lethality = 1,
          infectiousness = 1,
          visibility = 2
        ),
        Card$new(
          category = 'Insomnia',
          cost = 5,
          lethality = 2,
          infectiousness = 0,
          visibility = 2
        ),
        Card$new(
          category = 'Cysts',
          cost = 6,
          lethality = 1,
          infectiousness = 2,
          visibility = 5
        ),
        Card$new(
          category = 'Vomiting',
          cost = 3,
          lethality = 2,
          infectiousness = 2.5,
          visibility = 4
        ),
        Card$new(
          category = 'Anaemia',
          cost = 7,
          lethality = 5,
          infectiousness = 0,
          visibility = 5
        )
      )
    }
  ),
  public = list(
    initialize = function(n_cards = 10) {
      cli::cli_h2('creating card stack')
      private$shop_card_stack <- private$init_card_stack(n_cards)
    },
    makeCardUnavailable = function(card){
      sapply(private$shop_card_stack, function(x){
        if(x$id == card$id){
          x$setState('unavailable')
        }
      })
    },
    getCard = function(id){
      result <- sapply(private$shop_card_stack, function(x){
        if(x$id == id){
          return(x)
        }
      })
      result[[which(!is.null(result))]]
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