DiseaseManager <- R6::R6Class(
  "DiseaseManager",
  public = list(
    initialize = function(map_data) {
      self$setDeathProbability(0.001)
      self$setInfectionProbability(0.05)
      self$setRecoveryRate(0.01)
      #private$sproutFirstInfected(map_data)
    },
    sproutFirstInfected = function(map_data){
      top_countries <-
        map_data |>
        dplyr::select(-geometry) |>
        tibble::as_tibble() |>
        dplyr::arrange(desc(POP2005)) |>
        dplyr::top_n(10, POP2005) |>
        dplyr::pull(NAME)
      
      random_country <- sample(top_countries, 1)
      random_row <- which(map_data$NAME == random_country)
      map_data[random_row, ]$confirmed_cases <- 1
      return(map_data)
    },
    progressInfection = function(map_data, gameState){
      cli::cli_alert('progress infection')
      map_data <- private$recoverPopulation(map_data)
      map_data <- private$killPopulation(map_data)
      map_data <- private$spreadInfection(map_data, gameState)
      print('DONE')
      map_data
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
    }
  ),
  private = list(
    death_probability = NULL,
    infection_probability = NULL,
    recovery_rate = NULL,
    killPopulation = function(map_data,death_probability = self$getDeathProbability()){
      # of the infected population, there is a death_probability chance that any one person might die
      infected <- map_data$confirmed_cases
      
      new_deaths<- purrr::map_dbl(infected, function(total_infected){
        new_deaths <- sample(0:1, total_infected, replace = TRUE, prob = c(1-death_probability, death_probability))
        return(sum(new_deaths))
      })
      
      map_data$confirmed_deaths<- map_data$confirmed_deaths + new_deaths
      # when someone dies when infected, that is one less person with the infection
      # also remove from total population ?   
      map_data$confirmed_cases <- map_data$confirmed_cases - new_deaths
      return(map_data)
    },
    spreadInfection = function(map_data,gameState, infection_probability = self$getInfectionProbability()){
      in_country_spread_factor <- 1e-1
      cross_country_spread_factor <- 1e-3
      
      countries <- map_data$ISO3
      countries <- map_data[map_data$confirmed_cases > 0, ]$ISO3 |> 
        as.character()
      countries <- countries[!is.na(countries)]
      
      sapply(countries, function(country){
        total_infected <- map_data[map_data$ISO3 == country, ]$confirmed_cases
        total_population <- map_data[map_data$ISO3 == country, ]$POP2005
        if(total_population == 0){
          cli::cli_alert_warning('{country} has 0 population')
          return(NULL)
        }
        proportion_infected <- total_infected / total_population
        
        # In-country spread
        chance_of_spread <- in_country_spread_factor * (1 - proportion_infected)
        new_infections <- rbinom(1, total_infected, chance_of_spread)
        if(new_infections > 0) gameState()$earnDNAPoints(p = 1)
        map_data[map_data$ISO3 == country, ]$confirmed_cases <<- total_infected + new_infections
        
        # Cross-country spread
        bordering_countries <- private$borders[[country]]
        sapply(bordering_countries, function(bc){
          chance_of_spread <- sqrt(proportion_infected * cross_country_spread_factor)
          rbinom(1, 1, cross_country_spread_factor)
          new_infected <- sample(0:1, 1, prob = c(1 - chance_of_spread, chance_of_spread))
          if(new_infected == 0) return(NULL)
          
          existing_infected <- 
            map_data[map_data$ISO3 == bc, ]$confirmed_cases
          
          if(existing_infected > 0) return(NULL)
          gameState()$earnDNAPoints()
          country_row <- which(map_data$ISO3 == bc)
          map_data[country_row, ]$confirmed_cases <<- new_infected
          
          return(map_data)
        })
      })
      return(map_data)
    },
    recoverPopulation = function(map_data,recovery_rate = self$getRecoveryRate()){
      infected <- map_data$confirmed_cases
      
      new_recovered<- purrr::map_dbl(infected, function(total_infected){
        recovered <- sample(0:1, total_infected, replace = TRUE, prob = c(1-recovery_rate, recovery_rate))
        return(sum(recovered))
      })
      
      map_data$confirmed_recovered <- map_data$confirmed_recovered + new_recovered
      # when someone recovers after being infected, that is one less person with the infection
      map_data$confirmed_cases <- map_data$confirmed_cases - new_recovered
      return(map_data)
    }

  )
)