CardsManager <- R6::R6Class(
  "CardsManager",
  private = list(
    maximum_cards_on_display = 4,
    shop_card_stack = NULL,
    unblockCard = function(){
      done <- FALSE
      sapply(private$shop_card_stack, function(card){
        if(done) return(NULL)
        if(!card$isPurchased() && !card$isAvailable()){
          card$setState('available')
          done <<- TRUE
        }
      })
    },
    orderCards = function(cards){
      private$shop_card_stack <- 
        private$shop_card_stack[order(sapply(private$shop_card_stack, function(x) x$getCost()))]
    },
    setCardsInitialStatus = function(){
      counter <- 0
      result <- c()
      for(i in seq_along(private$shop_card_stack)){
        counter <- counter + 1
        card <- private$shop_card_stack[[i]]
        if(counter <= private$maximum_cards_on_display) card$setState('active')
        result[[i]] <- card
      }
      private$shop_card_stack <- result
    },
    init_card_stack = function(){
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
        ),
        Card$new(
          category = 'Pneumonia',
          cost = 10,
          lethality = 2,
          infectiousness = 4,
          visibility = 5
        ),
        Card$new(
          category = 'Paranoia',
          cost = 10,
          lethality = 1,
          infectiousness = 0,
          visibility = 5
        ),
        Card$new(
          category = 'Insanity',
          cost = 10,
          lethality = 2,
          infectiousness = 0,
          visibility = 6
        ),
        Card$new(
          category = 'Haemophilia',
          cost = 10, 
          lethality = 6,
          infectiousness = 6,
          visibility = 6
        ),
        Card$new(
          category = 'Fever',
          cost = 5,
          lethality = 2,
          infectiousness = 1,
          visibility = 2
        ),
        Card$new(
          category = 'Diarrhoea',
          cost = 4,
          lethality = 3,
          infectiousness = 7,
          visibility = 3
        ),
        Card$new(
          category = 'Pulmonary Fibrosis',
          cost = 10,
          lethality = 10,
          infectiousness = 1,
          visibility = 5
        ),
        Card$new(
          category = 'Pulmonary Oedema',
          cost = 8,
          lethality = 5,
          infectiousness = 4,
          visibility = 2
        ),
        Card$new(
          category = 'Tumours',
          cost = 30,
          lethality = 20,
          infectiousness = 1,
          visibility = 5
        ),
        Card$new(
          category = 'Immune Suppression',
          cost = 25,
          lethality = 12,
          infectiousness = 2,
          visibility = 2
        ),
        Card$new(
          category = 'Necrosis',
          cost = 25,
          lethality = 16,
          infectiousness = 8,
          visibility = 8
        ),
        Card$new(
          category = 'Hemorrhagic Shock',
          cost = 20,
          lethality = 10,
          infectiousness = 1,
          visibility = 6
        ),
        Card$new(
          category = 'Total Organ Failure',
          cost = 30,
          lethality = 32,
          infectiousness = 0,
          visibility = 16
        )
      )
    }
  ),
  public = list(
    initialize = function(n_cards = 10) {
      private$shop_card_stack <- private$init_card_stack()
      private$orderCards()
      private$setCardsInitialStatus()
    },
    buyCard = function(card){
      sapply(private$shop_card_stack, function(x){
        if(x$id == card$id){
          x$setState(c('unavailable', 'purchased'))
        }
      })
      private$unblockCard()
    },
    getCard = function(id){
      result <- sapply(private$shop_card_stack, function(x){
        if(x$id == id){
          return(x)
        }
      })
      result[!sapply(result, is.null)][[1]]
    },
    getCardStack = function(){
      private$shop_card_stack
    }
  )
)