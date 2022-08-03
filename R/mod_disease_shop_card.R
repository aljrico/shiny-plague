#' disease_shop_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_disease_shop_card_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("card_placeholder")),
  )
}

#' disease_shop_card Server Functions
#'
#' @noRd 
mod_disease_shop_card_server <- function(id, gameState, card_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    canAfford <- function(){
      gameState()$getDNAPoints() >= card()$getCost()
    }
    
    card <- reactive({
      gameState()$cardsManager$getCard(card_id)
    })
    # create card
    output$card_placeholder <- renderUI({
      if(is.null(card())) return(NULL)
      cli::cli_alert('render card')
      if(card()$isAvailable()){
        specs <- card()$getCard()
        diseaseShopCard(
          id = ns("card"), 
          category = specs$category, 
          cost = specs$cost, 
          lethality = specs$lethality, 
          infectiousness = specs$infectiousness, 
          visibility = specs$visibility, 
          state = specs$state, 
          disabled = canAfford()
        )
      }else{
        cli::cli_alert_warning("card not available")
        return(NULL)
      }
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
      
      # update the disease attributes
      gameState()$buyCard(card())

      # disable the card permanently 
      shinyjs::removeCssClass("card", class = "available")
      shinyjs::addCssClass("card", class = "unavailable")
    })
    
  })
}