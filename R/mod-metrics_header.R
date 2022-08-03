metrics_header_ui <- function(id = 'dna_score'){
  ns <- shiny::NS(id)
  tagList(
    tags$head(
      tags$style(
        ".metric-header {
      padding: .5em;
      text-align: center;
      margin-left: auto;
      margin-right: auto;
      border: 1px double;
      border-radius: 10px;
      }"  
      )
    ),
    
    fluidRow(
      column(
        width = 3,
        div(
          class = "metric-header",
          icon('dna'),
          textOutput(ns('score')),
          shinyBS::bsTooltip(ns('score'), "DNA Score",options = list(container = "body"))
        )
      ),
      column(
        width = 3,
        div(
          class = "metric-header",
          icon('biohazard'),
          textOutput(ns('infected')),
          shinyBS::bsTooltip(ns('infected'), "Total Infected Population")
        )
      ),
      column(
        width = 3,
        div(
          class = "metric-header",
          icon('skull-crossbones'),
          textOutput(ns('deaths')),
          shinyBS::bsTooltip(ns('deaths'), "Total Deaths")
        )
      ),
      column(
        width = 3,
        div(
          class = "metric-header",
          icon('briefcase-medical'),
          textOutput(ns('recovered')),
          shinyBS::bsTooltip(ns('recovered'), "Total Recovered")
          
        )
      )
    )
  )
}

metrics_header_server <- function(id = 'dna_score', gameState, mapManager){
  moduleServer(id, function(input, output, session){
    
    output$score <- renderText({
      cli::cli_alert('update score')
      prettyNum(gameState()$getDNAPoints(),big.mark=",", preserve.width="none")
    })
    
    output$infected <- renderText({
      cli::cli_alert('update infected')
      gameState() # create reactive dependency 
      prettyNum(mapManager$getTotalInfected(),big.mark=",", preserve.width="none")
    })
    
    output$deaths <- renderText({
      cli::cli_alert('update deaths')
      gameState() # create reactive dependency 
      prettyNum(mapManager$getTotalDeaths(),big.mark=",", preserve.width="none")
    })
    
    output$recovered <- renderText({
      cli::cli_alert('update recovered')
      gameState() # create reactive dependency 
      prettyNum(mapManager$getTotalRecovered(),big.mark=",", preserve.width="none")
    })
  })
}
