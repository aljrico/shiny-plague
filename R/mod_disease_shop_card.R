#' disease_shop_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_disease_shop_card_ui <- function(id, card){
  ns <- NS(id)
  specs <- card$getCard()
  tagList(
    diseaseShopCard(
      id = ns("card"), 
      category = specs$category, 
      cost = specs$cost, 
      lethality = specs$lethality, 
      infectiousness = specs$infectiousness, 
      visibility = specs$visibility, 
      state = specs$state, 
      disabled = FALSE
    )
  )
}


#' disease_shop_card Server Functions
#'
#' @noRd 
mod_disease_shop_card_server <- function(id, gameState, card_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    card <- CardsManager$new()$getCard(card_id)

    canAfford <- reactive({
      gameState()$getDNAPoints() >= card$getCost()
    })
    
    isCardAvailable <- reactive({
      gameState()$cardsManager$getCard(card_id)$isAvailable()
    })
    
    observe({
      if(isCardAvailable()) shinyjs::show('card')
      if(!isCardAvailable()) shinyjs::hide('card')
      shinyjs::toggleCssClass("card", class = "disabled", condition = !canAfford())
    })
    
    
    #check if player can afford the card item and toggle state accordingly
    # observeEvent(gameState(),{
    #   cli::cli_alert('check cards affordability')
    #   can_afford <- gameState()$getDNAPoints() >= card()$getCost()
    #   shinyjs::toggleCssClass("card", class = "unavailable", condition = !can_afford)
    #   shinyjs::toggleState(paste0(ns("card"), "_buy"), asis = TRUE, condition = can_afford)
    # })
    
    observeEvent(input$card_buy,{
      cli::cli_alert('buying card')
      gameState()$buyCard(card)
    })
    
  })
}