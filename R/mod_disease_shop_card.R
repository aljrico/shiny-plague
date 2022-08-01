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
mod_disease_shop_card_server <- function(id, gameState, Card){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # create card
    output$card_placeholder <- renderUI({
      
      do.call(diseaseShopCard, c(id = ns("card"), Card$getCard()))
    })
    
    
    #check if player can afford the card item and toggle state accordingly
    observeEvent(gameState()$getTicks(),{
      #### Replace this with getDNA when branches merged 
      can_afford <- gameState()$getScore() >= Card$getCost()

      shinyjs::toggleCssClass("card", class = "unavailable", condition = !can_afford)
      shinyjs::toggleState(paste0(ns("card"), "_buy"), asis = TRUE, condition = can_afford)

    })
    
    observeEvent(input$card_buy,{
      # subtract the cost of the card from the player
      # this method is not yet implemented
      #gameState()$spendDNA(card$getCost())
      
      # update the disease attributes
      gameState()$setDeathProbability(Card$getLethalityImpact())
      gameState()$setInfectionProbability(Card$getInfectiousnessImpact())
      gameState()$setRecoveryRate(Card$getVisibilityImpact())

      # disable the card permanently 
      shinyjs::removeCssClass("card", class = "available")
      shinyjs::addCssClass("card", class = "unavailable")
      Card$setState("unavailable")
    })
    
  })
}

## To be copied in the UI
# mod_disease_shop_card_ui("disease_shop_card_ui_1")

## To be copied in the server
# mod_disease_shop_card_server("disease_shop_card_ui_1")
