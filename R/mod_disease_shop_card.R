#' disease_shop_card UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_disease_shop_card_ui <- function(id, card, gameState){
  ns <- NS(id)
  specs <- card$getCard()
  canAfford <- gameState$getDNAPoints() >= card$getCost()
  if(!card$isAvailable()) return(NULL)
  div(
    diseaseShopCard(
      ns = ns, 
      category = specs$category, 
      cost = specs$cost, 
      lethality = specs$lethality, 
      infectiousness = specs$infectiousness, 
      visibility = specs$visibility, 
      state = specs$state, 
      disabled = !canAfford
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
    
    handleAvailability <- reactive({
      if(isCardAvailable()) shinyjs::show('container')
      if(!isCardAvailable()) shinyjs::hide('container')
      shinyjs::toggleCssClass("container", class = "disabled", condition = !canAfford())
    })
    
    observe({
      handleAvailability()
    })
    
    observeEvent(input$buy,{
      gameState()$buyCard(card)
      handleAvailability()
    })
    
  })
}