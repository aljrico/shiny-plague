#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      shinyjs::useShinyjs(),
      h1("shiny.plague"),
      mod_map_ui(),
      infected_score_ui()
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shiny.plague'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

cards <- data.frame(
  category = rep(c("Sneezing", "Coughing", "Pooping"), length.out = 10),
  cost = sample(1:100, 10),
  lethality = sample(-20:20, 10),
  infectiousness = sample(-20:20, 10),
  visibility = sample(-20:20, 10),
  state = rep(c("unavailable", "available", "cant_afford"), length.out = 10)
)

card_list <- do.call(Map, c(f= Card$new, cards)) 
names(card_list) <- NULL