#' Diease shop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
disease_shop_modal_ui <- function(id) {
  
  # create the namespace from the id
  ns <- NS(id)
  
}

#' disease_shop_modal Server Functions
#'
#' @noRd 
disease_shop_modal_server <- function(id,gameState, trigger){
  moduleServer(id, function(input, output, session) {
    cli::cli_alert('disease shop server')
    
    cards <- CardsManager$new()$getCardStack()
    card_ids <- sapply(cards, function(x) x$id)
    
    # load the namespace
    ns <- session$ns
    
    # for each card create the card ui module and card server module
    disease_shop_modal <- function(...) {
      
      # build the modal
      modalDialog(
        fluidPage(
          ...,
        ),
        
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
        )
        
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(trigger(), {
      cards_ui <- lapply(card_ids, function(id){
        mod_disease_shop_card_ui(ns(id), card = gameState()$cardsManager$getCard(id))
      })
      showModal(disease_shop_modal(cards_ui))
    })
    
    lapply(card_ids, function(card_id){
      mod_disease_shop_card_server(card_id, gameState, card_id)
    })
  })
}
