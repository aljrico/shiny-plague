#'
#'
#'
progressIndicators <- function(gameState){
  div(
    class = 'progress-indicators-container',
    div(
      class = 'progress-indicator',
      p(icon('dna'), "DNA Points: "),
      p(formatNumbers(gameState$getDNAPoints()))
    ),
    hr(style = "border: 0.5px solid #ccc; width: 100%; margin-top: 0px;"),
    div(
      class = 'progress-indicator',
      p(icon('user'), "Total Population: "),
      p(formatNumbers(gameState$getTotalPopulation()))
    ),
    div(
      class = 'progress-indicator',
      p(icon('biohazard'), "Infected Population: "),
      p(formatNumbers(gameState$getTotalInfected()))
    ),
    div(
      class = 'progress-indicator',
      p(icon('skull-crossbones'), "Dead Population: "),
      p(formatNumbers(gameState$getTotalDeaths()))
    ),
    div(
      class = 'progress-indicator',
      p(icon('shield-virus'), "Immune Population: "),
      p(formatNumbers(gameState$getImmunePopulation()))
    )
  )
}