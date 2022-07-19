GameState <- R6::R6Class(
  "GameState",
  private = list(
    map_data = NULL,
    ticks = NULL,
    score = NULL,
    health = NULL,
    win = NULL,
    lose = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    count = 0,
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    initializeData = function(){
      data('map_data')
      map_data$geometry <- NULL
      class(map_data) <- "data.frame"
      top_countries <-
        map_data |>
        dplyr::arrange(desc(POP2005)) |>
        dplyr::top_n(10, POP2005) |>
        dplyr::pull(NAME)
      
      random_country <- sample(top_countries, 1)
      random_row <- which(map_data$NAME == random_country)
      # map_data <- map_data[map_data$POP2005 > 0, ]
      map_data[random_row, ]$confirmed_cases <- 1
      private$map_data <- map_data
    },
    killPopulation = function(death_probability = 0.001){
      # of the infected population, there is a death_probability chance that any one person might die
      infected <- private$map_data$confirmed_cases
      
      new_deaths<- purrr::map_dbl(infected, function(total_infected){
        new_deaths <- sample(0:1, total_infected, replace = TRUE, prob = c(1-death_probability, death_probability))
        return(sum(new_deaths))
      })
      
      
      private$map_data$confirmed_deaths<- private$map_data$confirmed_deaths + new_deaths
      # when someone dies when infected, that is one less person with the infection
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_deaths
      
    },
    spreadInfection = function(infection_probability = 0.05){
      infected <- private$map_data$confirmed_cases
      
      new_infected <- purrr::map_dbl(infected, function(total_infected){
        if (total_infected == 0 ){
          # random term to spread infections
          total_infected <- sample(0:1, 1, prob = c(0.99, 0.01))
        }
        new_infections <- sample(0:1, total_infected, replace = TRUE, prob = c(1-infection_probability, infection_probability))
        return(total_infected + sum(new_infections))
      })
      
      private$map_data$confirmed_cases <- new_infected
      # make sure we don't have more infected than total population
      private$map_data$confirmed_cases <- pmin(private$map_data$confirmed_cases,
                                               private$map_data$POP2005)
      
    },
    recoverPopulation = function(recovery_rate = 0.01){
      infected <- private$map_data$confirmed_cases
      
      new_recovered<- purrr::map_dbl(infected, function(total_infected){
        recovered <- sample(0:1, total_infected, replace = TRUE, prob = c(1-recovery_rate, recovery_rate))
        return(sum(recovered))
      })
      
      private$map_data$confirmed_recovered <- private$map_data$confirmed_recovered + new_recovered
      # when someone recovers after being infected, that is one less person with the infection
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_recovered
    }
    
  ),
  public = list(
    initialize = function() {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$initializeData()
      private$win <- FALSE
      private$lose <- FALSE
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
      cat("Score:", private$score, "/n",
          "Health:", private$health)
    },
    getMapData = function(){
      private$map_data
    },
    progressInfection = function(){
      private$recoverPopulation()
      private$killPopulation()
      private$spreadInfection()
    },
    # spreadInfection = function(){
    #   infection_rate <- 0.1
    #   populations <- private$map_data$POP2005
    #   infected <- private$map_data$confirmed_cases
    #   infected_proportion <- infected / populations
    #   new_infected <-
    #    round((1 - infected_proportion ) * infected * (0.5 + infection_rate) * runif(length(infected)))
    # 
    #   private$map_data$confirmed_cases <-
    #     infected + new_infected
    #   private$map_data |>
    #     as.data.frame() |>
    #     dplyr::filter(confirmed_cases > 0) |>
    #     print()
    # },
    addTick = function(){
      private$ticks <- private$ticks + 1
    },
    getTicks = function(){
      private$ticks
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
