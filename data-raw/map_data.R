## code to prepare `map_data` dataset goes here

data("map_data", envir = environment())

# add row numbers to map_data to be used to join with borders
map_data <- map_data |>
  dplyr::mutate(inx = dplyr::row_number()) 

iso_inx_map <- map_data |>
  dplyr::select(ISO3, inx)
iso_inx_map$geometry <- NULL

# add borders
sf::sf_use_s2(FALSE)
borders <- sf::st_intersects(map_data,remove_self= TRUE ) |>
  tibble::enframe(name = "inx") |>  # convert list to tibble
  tidyr::unnest(value) |> # make data longer
  dplyr::rename(borders_with = value) |>
  dplyr::mutate(borders_with = iso_inx_map[borders_with, "ISO3"])
 

borders_wide <- borders |>
  dplyr::mutate(value = 1) |>
  tidyr::pivot_wider(names_from = borders_with,
                     names_prefix = "borders_with_",
                     values_from = value, 
                     values_fill = 0)


map_data <- dplyr::left_join(map_data, borders_wide) |>
  dplyr::mutate(dplyr::across(tidyselect::starts_with("borders_with_"), ~tidyr::replace_na(.x, 0)))

#map_data$geometry <- NULL
# create border mapping 
countries <- map_data$ISO3

borders <- lapply(countries, function(country) {
  map_data |>
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

usethis::use_data(map_data, overwrite = TRUE)
usethis::use_data(borders, overwrite = TRUE)

