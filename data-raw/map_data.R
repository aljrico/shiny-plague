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


map_data <- left_join(map_data, borders_wide) |>
  dplyr::mutate(dplyr::across(tidyselect::starts_with("borders_with_"), ~tidyr::replace_na(.x, 0)))

map_data$geometry <- NULL
usethis::use_data(map_data, overwrite = TRUE)

