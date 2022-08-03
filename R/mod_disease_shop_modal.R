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
disease_shop_modal_server <- function(id,gameState, trigger, cardStack){
  moduleServer(id, function(input, output, session) {
    cli::cli_alert('disease shop server')
    
    cards <- cardStack$getCardStack()
    card_indices <- 1:length(cards)
    
    # load the namespace
    ns <- session$ns
    
    # for each card create the card ui module and card server module
    disease_shop_modal <- function(...) {
      
      # build the modal
      modalDialog(
        tagList(
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
      cli::cli_alert('show shop modal')
      cards_ui <- purrr::map(card_indices, function(id){ mod_disease_shop_card_ui(ns(id))})
      
      showModal(disease_shop_modal(cards_ui))
    })
    
    lapply(card_indices, function(index){
      mod_disease_shop_card_server(index, gameState, cards[[index]])
    })
    # purrr::walk(card_indices, function(index) {mod_disease_shop_card_server(index, gameState, cards[[index]])})
  })
}
