GameState <- R6::R6Class(
  "GameState",
  private = list(
    ticks = NULL,
    score = NULL,
    health = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
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
    addTick = function(){
      private$ticks <- private$ticks + 1
    },
    getTicks = function(){
      private$ticks
    },
    changeScore = function(add_score) {
      private$score <- private$score + add_score
      private$invalidate()
    },
    getScore = function() {
      private$score
    },
    changeHealth = function(add_health) {
      private$health <- private$health + add_health
      private$invalidate()
    },
    getHealth = function() {
      private$health
    }
  )
)
