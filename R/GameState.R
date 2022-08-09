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
    lethality = NULL,
    infectiousness = NULL,
    visibility = NULL,
    recovery_rate = NULL,
    count = 0,
    date = lubridate::ymd("1970-01-01"),
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },
    initializeData = function() {
      cli::cli_alert("initialize data")
      data("map_data")
      private$map_data <- map_data
      private$sproutFirstInfected()
      private$createBorderMapping()
    },
    killPopulation = function() {
      cli::cli_alert("kill population")
      # of the infected population, there is a lethality chance that any one person might die
      infected <- private$map_data$confirmed_cases

      new_deaths <- purrr::map_dbl(infected, function(total_infected) {
        new_deaths <- sample(0:1, total_infected, replace = TRUE, prob = c(1 - private$lethality, private$lethality))
        return(sum(new_deaths))
      })

      private$map_data$confirmed_deaths <- private$map_data$confirmed_deaths + new_deaths
      # when someone dies when infected, that is one less person with the infection
      # also remove from total population ?
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_deaths
    },
    spreadInfection = function() {
      cli::cli_h3("spread infection")
      cross_country_spread_factor <- 1e-3

      countries <- private$map_data$ISO3
      countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |>
        as.character()
      countries <- countries[!is.na(countries)]

      sapply(countries, function(country) {
        total_infected <- private$map_data[private$map_data$ISO3 == country, ]$confirmed_cases
        total_population <- private$map_data[private$map_data$ISO3 == country, ]$POP2005
        if (total_population == 0) {
          cli::cli_alert_warning("{country} has 0 population")
          return(NULL)
        }
        proportion_infected <- min(total_infected / total_population, 1)

        # In-country spread
        cli::cli_alert("in-country spread")
        chance_of_spread <- getInCountrySpreadChance(
          private$infectiousness,
          proportion_infected
        )
        new_infections <- rbinom(1, total_infected, chance_of_spread)
        if (new_infections > 0) self$earnDNAPoints(p = 0.5)
        if(total_infected + new_infections >= total_population){
          private$map_data[private$map_data$ISO3 == country, ]$confirmed_cases <- total_population
        }else{
          private$map_data[private$map_data$ISO3 == country, ]$confirmed_cases <- total_infected + new_infections 
        }

        # Cross-country spread
        cli::cli_alert("cross-country spread")
        bordering_countries <- private$borders[[country]]
        sapply(bordering_countries, function(bc) {
          chance_of_spread <- getCrossCountrySpreadChance(
            private$infectiousness,
            proportion_infected
          )
          new_infected <- rbinom(1, 1, chance_of_spread)
          if (new_infected == 0) {
            return(NULL)
          }

          existing_infected <-
            private$map_data[private$map_data$ISO3 == bc, ]$confirmed_cases

          if (existing_infected > 0) {
            return(NULL)
          }
          self$earnDNAPoints(p = 1)
          country_row <- which(private$map_data$ISO3 == bc)
          private$map_data[country_row, ]$confirmed_cases <- new_infected
        })
      })
    },
    recoverPopulation = function(recovery_rate = self$getRecoveryRate()) {
      cli::cli_alert("recover population")
      infected <- private$map_data$confirmed_cases

      new_recovered <- purrr::map_dbl(infected, function(total_infected) {
        recovered <- sample(0:1, total_infected, replace = TRUE, prob = c(1 - recovery_rate, recovery_rate))
        return(sum(recovered))
      })

      private$map_data$confirmed_recovered <- private$map_data$confirmed_recovered + new_recovered
      # when someone recovers after being infected, that is one less person with the infection
      private$map_data$confirmed_cases <- private$map_data$confirmed_cases - new_recovered
    },
    getBorders = function(country) {
      has_border <- function(x) {
        if (x == 1) {
          return(TRUE)
        }
        return(FALSE)
      }
      # borders <- private$map_data |>
      #   dplyr::filter(ISO3 == country) |>
      #   dplyr::select(tidyr::starts_with("borders_with_")) |>
      #   purrr::keep(~ has_border(.x))
      # dplyr::select(where(~ has_border(.x)))
      cols <- grep("borders_with_", names(private$map_data))
      private$map_data |>
        dplyr::filter(ISO3 == country) |>
        dplyr::pull()
      borders <- private$map_data[ISO3 == country, ..cols] |>
        purrr::keep(~ has_border(.x))

      borders <- names(borders) |>
        stringr::str_remove("borders_with_")
    },
    createBorderMapping = function() {
      countries <- private$map_data$ISO3

      borders <- lapply(countries, function(country) {
        private$map_data |>
          dplyr::select(-geometry) |>
          tibble::as_tibble() |>
          dplyr::filter(ISO3 == country) |>
          dplyr::select(dplyr::contains("borders_with")) |>
          tidyr::pivot_longer(tidyr::everything()) |>
          dplyr::filter(value > 0) |>
          dplyr::mutate(name = name |> stringr::str_remove("borders_with_")) |>
          dplyr::pull(name) |>
          unique()
      })

      names(borders) <- countries
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
      private$score <- 0
      private$health <- 100

      self$setDeathProbability(0)
      self$setInfectionProbability(0)
      self$setRecoveryRate(0)
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
      if("try-error" %in% class(canAfford)) return(NULL)
      self$increaseLethality(by = card$getLethalityImpact() * increase_modifier)
      self$increaseVisibility(by = card$getVisibilityImpact() * increase_modifier)
      self$increaseInfectiousness(by = card$getInfectiousnessImpact() * increase_modifier)
      self$cardsManager$makeCardUnavailable(card)
      private$invalidate()
    },
    increaseLethality = function(by){
      private$lethality <- private$lethality + by
    }, 
    increaseVisibility = function(by){
      private$visibility <- private$visibility + by  
    },
    increaseInfectiousness = function(by){
      private$infectiousness <- private$infectiousness + by
    },
    setDeathProbability = function(lethality) {
      private$lethality <- lethality
    },
    setInfectionProbability = function(infectiousness) {
      private$infectiousness <- infectiousness
    },
    setRecoveryRate = function(recovery_rate) {
      private$recovery_rate <- recovery_rate
    },
    getLethality = function() {
      private$lethality
    },
    getInfectiousness = function() {
      private$infectiousness
    },
    getVisibility = function(){
      private$visibility
    },
    getRecoveryRate = function() {
      private$recovery_rate
    },
    getMapData = function() {
      private$map_data
    },
    progressInfection = function() {
      cli::cli_alert("progress infection")
      private$recoverPopulation()
      private$killPopulation()
      private$spreadInfection()
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
    getTotalRecovered = function() {
      sum(private$map_data$confirmed_recovered)
    },
    increaseDate = function(){
      private$date <- private$date + 1
    },
    getDate = function(){
      private$date
    }
  )
)
