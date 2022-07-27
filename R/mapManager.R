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
                        getMapData = function(){
                          private$map_data
                        }
                      )
)