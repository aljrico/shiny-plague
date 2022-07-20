GameState <- R6::R6Class(
  "GameState",
  private = list(
    map_data = NULL,
    country_border_mapping = NULL,
    ticks = NULL,
    score = NULL,
    health = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    death_probability = NULL,
    infection_probability = NULL,
    recovery_rate = NULL,
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
      private$map_data <- data.table::setDT(map_data)
    },
    killPopulation = function(death_probability = self$getDeathProbability()){
      # of the infected population, there is a death_probability chance that any one person might die
      infected <- private$map_data$confirmed_cases
      
      new_deaths<- purrr::map_dbl(infected, function(total_infected){
        new_deaths <- sample(0:1, total_infected, replace = TRUE, prob = c(1-death_probability, death_probability))
        return(sum(new_deaths))
      })
      
      private$map_data$confirmed_deaths<- private$map_data$confirmed_deaths + new_deaths
      # when someone dies when infected, that is one less person with the infection
      # also remove from total population ?   
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_deaths
      
    },
    spreadInfection = function(infection_probability = self$getInfectionProbability()){
      countries <- private$map_data$ISO3
      
      new_infected <- purrr::map_dbl(countries, function(country){
        total_infected <- private$map_data[private$map_data$ISO3 == country, confirmed_cases]
        if (total_infected == 0 ){
          # if any of the bordering countries have higher than 10% of their population infected, this will
          # increase the chance of the disease hopping borders
          # otherwise there is a small random probablity that the disease will spread into new territory 
          borders_with <- private$country_border_mapping[[country]]
          
          # neighbour_cases <- private$map_data |>
          #   dplyr::select(ISO3, confirmed_cases, POP2005) |>
          #   dplyr::filter(ISO3 %in% borders_with) |>
          #   dplyr::mutate(prop_infected = confirmed_cases / POP2005) |>
          #   dplyr::filter(prop_infected > 0.1)
          
          neighbour_cases <- private$map_data[ISO3 %in% borders_with, .(prop_infected =  confirmed_cases / POP2005)][
            prop_infected > 0.1]
          
          infected_neighbours <- nrow(neighbour_cases)
          infected_neighbours_weighting <-  0.01
          random_term<- 0.001
          chance_of_spread <- infected_neighbours * infected_neighbours_weighting + random_term
          
          total_infected <- sample(0:1, 1, prob = c(1 - chance_of_spread, chance_of_spread))
        }
        new_infections <- sample(0:1, total_infected, replace = TRUE, prob = c(1-infection_probability, infection_probability))
        return(total_infected + sum(new_infections))
      })
      
      private$map_data$confirmed_cases <- new_infected
      # make sure we don't have more infected than total population
      private$map_data$confirmed_cases <- pmin(private$map_data$confirmed_cases,
                                               private$map_data$POP2005)
      
    },
    recoverPopulation = function(recovery_rate = self$getRecoveryRate()){
      infected <- private$map_data$confirmed_cases
      
      new_recovered<- purrr::map_dbl(infected, function(total_infected){
        recovered <- sample(0:1, total_infected, replace = TRUE, prob = c(1-recovery_rate, recovery_rate))
        return(sum(recovered))
      })
      
      private$map_data$confirmed_recovered <- private$map_data$confirmed_recovered + new_recovered
      # when someone recovers after being infected, that is one less person with the infection
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_recovered
    },
    getBorders = function(country){
      has_border <- function(x){
        if(x == 1) return(TRUE)
        return(FALSE)
      }
      # borders <- private$map_data |>
      #   dplyr::filter(ISO3 == country) |>
      #   dplyr::select(tidyr::starts_with("borders_with_")) |>
      #   purrr::keep(~ has_border(.x))
      #dplyr::select(where(~ has_border(.x)))
      cols <- grep("borders_with_", names(private$map_data))
      borders <- private$map_data[ISO3 == country, ..cols] |>
        purrr::keep(~ has_border(.x))
      
      borders <- names(borders) |>
        stringr::str_remove("borders_with_")
    },
    createBorderMapping = function(){
      countries <- private$map_data$ISO3
      borders <- purrr::map(countries, ~ private$getBorders(.x))
      return(setNames(as.list(borders), countries))
    }
  ),
  public = list(
    initialize = function() {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$initializeData()
      private$country_border_mapping <- private$createBorderMapping()
      private$score <- 0
      private$ticks <- 0
      private$health <- 100
      
      self$setDeathProbability(0.001)
      self$setInfectionProbability(0.05)
      self$setRecoveryRate(0.01)
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
    setDeathProbability = function(death_probability){
      private$death_probability <- death_probability
    },
    setInfectionProbability = function(infection_probability){
      private$infection_probability <- infection_probability
    },
    setRecoveryRate = function(recovery_rate){
      private$recovery_rate <- recovery_rate
    },
    getDeathProbability = function(){
      private$death_probability 
    },
    getInfectionProbability = function(){
      private$infection_probability
    },
    getRecoveryRate = function(){
      private$recovery_rate
    },
    getMapData = function(){
      private$map_data
    },
    progressInfection = function(){
      private$recoverPopulation()
      private$killPopulation()
      private$spreadInfection()
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
