GameState <- R6::R6Class(
  "GameState",
  private = list(
    score = NULL,
    health = NULL,
    win = NULL,
    lose = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    count = 0
  ),
  public = list(
    initialize = function() {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$win <- FALSE
      private$lose <- FALSE
      private$score <- 0
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
      cat("Score:", private$score, "/n",
          "Health:", private$health)
    },
    checkWin = function(){
      if(private$score > 50){
        private$win <- TRUE
      } else {
        private$win <- FALSE
      }
      return(private$win)
    },
    checkLose = function(){
      if(private$health < 0){
        private$lose <- TRUE
      } else {
        private$lose <- FALSE
      }
      return(private$lose)
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
