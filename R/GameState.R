GameState <- R6::R6Class(
  "GameState",
  private = list(
    ticks = NULL,
    score = NULL,
    health = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    dna_points_probability = 0.1,
    dna_points = 0,
    count = 0,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    }
  ),
  public = list(
    initialize = function() {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$score <- 0
      private$ticks <- 0
      private$health <- 100
    },
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    },
    print = function() {
      #cli::cli_alert_info(paste0("Infected Countries: ", infected_countries))
      #cli::cli_alert_info(paste0("Total Infected:", total_infected))
      #cli::cat_rule()
      cli::cli_alert("implement a GameState print method")
    },
    earnDNAPoints = function(n = 1, p = private$dna_points_probability){
      print("adding points")
      new_points <- rbinom(1, n, p)
      private$dna_points <- private$dna_points + new_points
      private$invalidate()
    },
    getDNAPoints = function(){
      private$dna_points
    }
  )
)
