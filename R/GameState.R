GameState <- R6::R6Class(
  "GameState",
  private = list(
    map_data = NULL,
    borders = NULL,
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
      private$map_data <- map_data[map_data$POP2005 > 0, ]
      private$sproutFirstInfected()
      private$createBorderMapping()
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
      
      in_country_spread_factor <- 1e-1
      cross_country_spread_factor <- 1e-3
      
      countries <- private$map_data$ISO3
      countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |> 
        as.character()
      countries <- countries[!is.na(countries)]
      
      sapply(countries, function(country){
        total_infected <- private$map_data[private$map_data$ISO3 == country, ]$confirmed_cases
        proportion_infected <- total_infected / private$map_data[private$map_data$ISO3 == country, ]$POP2005
        
        # In-country spread
        chance_of_spread <- in_country_spread_factor * (1 - proportion_infected)
        new_infections <- rbinom(1, total_infected, chance_of_spread)
        private$map_data[private$map_data$ISO3 == country, ]$confirmed_cases <- total_infected + new_infections
        
        # Cross-country spread
        bordering_countries <- private$borders[[country]]
        sapply(bordering_countries, function(bc){
          chance_of_spread <- sqrt(proportion_infected * cross_country_spread_factor)
          rbinom(1, 1, cross_country_spread_factor)
          new_infected <- sample(0:1, 1, prob = c(1 - chance_of_spread, chance_of_spread))
          if(new_infected == 0) return(NULL)
          
          existing_infected <- 
            private$map_data[private$map_data$ISO3 == bc, ]$confirmed_cases
          
          if(existing_infected > 0) return(NULL)
          country_row <- which(private$map_data$ISO3 == bc)
          private$map_data[country_row, ]$confirmed_cases <- new_infected
        })
      })
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
      private$map_data |> 
        dplyr::filter(ISO3 == country) |> 
        dplyr::pull()
      borders <- private$map_data[ISO3 == country, ..cols] |>
        purrr::keep(~ has_border(.x))
      
      borders <- names(borders) |>
        stringr::str_remove("borders_with_")
    },
    createBorderMapping = function(){
      countries <- private$map_data$ISO3

      borders <- lapply(countries, function(country){
        private$map_data |> 
          dplyr::select(-geometry) |> 
          tibble::as_tibble() |> 
          dplyr::filter(ISO3 == country) |>
          dplyr::select(dplyr::contains('borders_with')) |> 
          tidyr::pivot_longer(tidyr::everything()) |> 
          dplyr::filter(value > 0) |> 
          dplyr::mutate(name = name |> stringr::str_remove('borders_with_')) |> 
          dplyr::pull(name) |> 
          unique()
      })
      
      names(borders) <- countries
      private$borders <- borders
    },
    sproutFirstInfected = function(){
      top_countries <-
        private$map_data |>
        dplyr::select(-geometry) |>
        tibble::as_tibble() |> 
        dplyr::arrange(desc(POP2005)) |>
        dplyr::top_n(10, POP2005) |>
        dplyr::pull(NAME)
      
      random_country <- sample(top_countries, 1)
      random_row <- which(private$map_data$NAME == random_country)
      private$map_data[random_row, ]$confirmed_cases <- 1
    }
    
  ),
  public = list(
    initialize = function() {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$initializeData()
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
      infected_countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |> as.character() |>  unique()
      total_infected <- private$map_data$confirmed_cases |> sum()
      
      cli::cli_alert_info(paste0("Infected Countries: ", infected_countries))
      cli::cli_alert_info(paste0("Total Infected:", total_infected))
      cli::cat_rule()
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
      self$print()
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
