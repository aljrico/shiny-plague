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
      p(icon('briefcase-medical'), "Recovered Population: "),
      p(formatNumbers(gameState$getTotalRecovered()))
    )
  )
}