GameState <- R6::R6Class(
  "GameState",
  private = list(
    map_data = NULL,
    borders = NULL,
    country_border_mapping = NULL,
    score = NULL,
    health = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    dna_points_probability = 0.1,
    dna_points = 0,
    lethality = 0,
    infectiousness = 0,
    visibility = 0,
    recovery_rate = NULL,
    airborne_bonus = 0,
    medical_progress = 0.01,
    count = 0,
    date = lubridate::ymd("1994-12-23"),
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    initializeData = function() {
      data("map_data", envir = environment())
      private$map_data <- map_data
      private$sproutFirstInfected()
      private$createBorderMapping()
      private$map_data$immune_population <- 0
    },
    killPopulation = function() {
      death_chance <- getDeathChance(self$getLethality())
      if(death_chance == 0) return(NULL)
      infected <- private$map_data$confirmed_cases
      
      new_deaths <- purrr::map_dbl(infected, function(total_infected) {
        rbinom(1, total_infected, death_chance)
      })
      
      private$map_data$confirmed_deaths <- pmin(private$map_data$confirmed_deaths + new_deaths, private$map_data$POP2005)
      # when someone dies when infected, that is one less person with the infection
      # also remove from total population ?
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_deaths
    },
    spreadInfection = function() {
      # in country spread
      total_infected <- private$map_data$confirmed_cases
      total_population <- private$map_data$POP2005
      proportion_infected <- total_infected / total_population
      chance_of_spread <- getInCountrySpreadChance(
        private$infectiousness,
        proportion_infected)
      
      new_infected <- private$map_data |>
        dplyr::select(confirmed_cases)|>
        dplyr::rowwise() |>
        dplyr::mutate(new_infected = dplyr::case_when(
          confirmed_cases == 0 ~ confirmed_cases ,
          TRUE ~ as.numeric(rbinom(1, confirmed_cases, chance_of_spread)))) |>
        dplyr::pull(new_infected)
      
      # confirmed cases can't be more than the total population 
      private$map_data$confirmed_cases <- pmin(total_infected + new_infected, total_population)
      # number of new infections will determine how many dna points are rewarded
      num_new_infections <- sum(new_infected != 0)
      if (num_new_infections !=0) {
        purrr::walk(1:num_new_infections, ~self$earnDNAPoints(p = 0.9))
      }
      
      countries <- private$map_data$ISO3
      countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |>
        as.character()
      countries <- countries[!is.na(countries)]
      #Cross-country spread
      sapply(countries, function(country) {
        # Cross-country spread
        bordering_countries <- private$borders[[country]]
        sapply(bordering_countries, function(bc) {
          country_row <- which(private$map_data$ISO3 == bc)
          
          chance_of_spread <- getCrossCountrySpreadChance(
            private$infectiousness,
            proportion_infected[[country_row]]
          )
          
          new_infected <- rbinom(1, 1, chance_of_spread)
          if (new_infected == 0) {
            return(NULL)
          }
          
          existing_infected <- total_infected[[country_row]]
          
          if (existing_infected > 0) {
            return(NULL)
          }
          self$earnDNAPoints(p = 1)
          private$map_data[country_row, ]$confirmed_cases <- new_infected
        })
      })
      
      # Airborne Spread
      empty_countries <- private$map_data[private$map_data$confirmed_cases == 0, ]$ISO3 |>
        as.character()
      empty_countries <- empty_countries[!is.na(empty_countries)]
      
      spread_chance <- getAirborneSpreadChance(
        infectiousness = self$getInfectiousness(),
        airborne_bonus = self$getAirborneBonus()
      )
      
      if (runif(1) < spread_chance) {
        countries_pool <-
          private$map_data |>
          dplyr::filter(POP2005 > 0) |>
          dplyr::filter(confirmed_cases == 0) |>
          dplyr::arrange(desc(POP2005)) |>
          dplyr::top_n(10, POP2005) |>
          dplyr::pull(ISO3) |>
          as.character() |>
          unique()
        if (length(countries_pool) == 0) {
          return(NULL)
        }
        country_row <- which(private$map_data$ISO3 == sample(countries_pool, 1))
        private$map_data[country_row, ]$confirmed_cases <- 1
      }
    },
    recoverPopulation = function() {
      infected <- private$map_data$confirmed_cases
      recovery_chance <- getRecoveryChance(self$getMedicalProgress())
      
      new_recovered <- purrr::map_dbl(infected, function(total_infected) {
        rbinom(1, total_infected, recovery_chance)
      })
      
      private$map_data$confirmed_recovered <- private$map_data$confirmed_recovered + new_recovered
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_recovered
    },
    createBorderMapping = function() {
      data("borders")
      private$borders <- borders
    },
    sproutFirstInfected = function() {
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
    cardsManager = NULL,
    initialize = function() {
      # Until someone calls $reactive(), private$reactiveDep() is a no-op. Need
      # to set it here because if it's set in the definition of private above, it will
      # be locked and can't be changed.
      private$reactiveDep <- function(x) NULL
      private$initializeData()
      self$setDeathProbability(0)
      self$setInfectionProbability(0)
      self$cardsManager <- CardsManager$new()
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
      infected_countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |>
        as.character() |>
        unique()
      total_infected <- private$map_data$confirmed_cases |> sum()
      
      cli::cli_alert_info(paste0("Infected Countries: ", infected_countries))
      cli::cli_alert_info(paste0("Total Infected:", total_infected))
      cli::cat_rule()
    },
    buyCard = function(card) {
      increase_modifier <- 0.1
      canAfford <- try(self$spendDNAPoints(card$getCost()))
      if ("try-error" %in% class(canAfford)) {
        return(NULL)
      }
      self$increaseLethality(by = card$getLethalityImpact() * increase_modifier)
      self$increaseVisibility(by = card$getVisibilityImpact() * increase_modifier)
      self$increaseInfectiousness(by = card$getInfectiousnessImpact() * increase_modifier)
      self$cardsManager$makeCardUnavailable(card)
      private$invalidate()
    },
    increaseLethality = function(by) {
      private$lethality <- private$lethality + by
    },
    increaseVisibility = function(by) {
      private$visibility <- private$visibility + by
    },
    increaseInfectiousness = function(by) {
      private$infectiousness <- private$infectiousness + by
    },
    setDeathProbability = function(lethality) {
      private$lethality <- lethality
    },
    setInfectionProbability = function(infectiousness) {
      private$infectiousness <- infectiousness
    },
    getLethality = function() {
      private$lethality
    },
    getInfectiousness = function() {
      private$infectiousness
    },
    getVisibility = function() {
      private$visibility
    },
    getMedicalProgress = function(){
      private$medical_progress
    },
    getMapData = function() {
      private$map_data
    },
    getAirborneBonus = function() {
      private$airborne_bonus
    },
    progressInfection = function() {
      private$recoverPopulation()
      private$killPopulation()
      private$spreadInfection()
      private$invalidate()
    },
    increaseMedicalProgress = function(){
      multiplying_factor <- max(0.01, self$getVisibility() / 10)
      private$medical_progress <- private$medical_progress * (1 + multiplying_factor)
      private$invalidate()
    },
    earnDNAPoints = function(n = 1, p = private$dna_points_probability) {
      new_points <- rbinom(1, n, p)
      private$dna_points <- private$dna_points + new_points
      private$invalidate()
    },
    spendDNAPoints = function(amount) {
      if (private$dna_points < amount) {
        cli::cli_alert_danger("Trying to spend more DNA points than what we have")
        stop("Trying to spend more DNA points than what we have")
      }
      private$dna_points <- private$dna_points - amount
    },
    getDNAPoints = function() {
      private$dna_points
    },
    getTotalInfected = function() {
      sum(private$map_data$confirmed_cases)
    },
    getTotalPopulation = function() {
      sum(private$map_data$POP2005)
    },
    getTotalDeaths = function() {
      sum(private$map_data$confirmed_deaths)
    },
    getImmunePopulation = function(){
      sum(private$map_data$immune_population)
    },
    getTotalRecovered = function() {
      sum(private$map_data$confirmed_recovered)
    },
    increaseDate = function() {
      private$date <- private$date + 1
    },
    getDate = function() {
      private$date
    }
  )
)
