MapManager <- R6::R6Class("MapManager",
                      private = list(
                        disease = NULL,
                        borders = NULL,
                        map_data = NULL,
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
                        initializeMap = function(){
                          data('map_data')
                          private$map_data <- map_data
                          private$createBorderMapping()
                        }
                      ),

                      public = list(
                        initialize = function() {
                          private$initializeMap()
                          private$disease <- DiseaseManager$new(private$map_data)
                        },
                        updateMapData = function(new_map_data){
                          private$map_data <- new_map_data
                        },
                        getMapData = function(){
                          private$map_data
                        },
                        getInfectedCountries =function(){
                          infected_countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |> as.character() |>  unique()
                        },
                        getTotalInfected =function(){
                          total_infected <- private$map_data$confirmed_cases |> sum()
                        },
                        print = function() {
                          infected_countries <- private$map_data[private$map_data$confirmed_cases > 0, ]$ISO3 |> as.character() |>  unique()
                          total_infected <- private$map_data$confirmed_cases |> sum()
                          
                          cli::cli_alert_info(paste0("Infected Countries: ", infected_countries))
                          cli::cli_alert_info(paste0("Total Infected:", total_infected))
                          cli::cat_rule()
                        }
                      )
)