# c(
#   "dplyr",
#   "ggplot2",
#   "purrr",
#   "readr",
#   "magrittr"
#   ) |> purrr::map(usethis::use_package)

library(ggplot2)

set.seed(123)
data <-
  "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/Einzelmessstellen" |>
    list.files(pattern = ".txt", full.names = TRUE) |>
    sample(5) |>
    purrr::map_dfr(readr::read_csv2,
                   col_types = "ciiciicdd",
                   locale = readr::locale(decimal_mark = ".", grouping_mark = ",")) |>
  janitor::clean_names() |>
  dplyr::select(mest_id, datum, gw_nn) |>
  dplyr::rename(well_id = mest_id,
                date = datum,
                gwl = gw_nn) |>
  dplyr::mutate(date = lubridate::dmy_hms(date),
                date = as.Date(date),
                gwl = as.numeric(gwl)) |>
  dplyr::mutate(year = lubridate::year(date), .after = date) |>
  dplyr::group_by(well_id) |>
  dplyr::arrange(date) |>
  dplyr::ungroup() |>
  tidyr::drop_na(gwl)

# Check temporal resolution
data |>
  dplyr::group_by(well_id) |>
  dplyr::mutate(timediff = date - dplyr::lag(date),
                timediff = as.numeric(timediff)) |>
  ggplot(aes(timediff)) +
  geom_histogram(fill = "lightblue", colour = "white") +
  scale_x_log10() +
  theme_minimal() +
  scale_y_continuous(trans = "pseudo_log")

# Check data availability
data |>
  overviewR::overview_heat(well_id, year, perc = TRUE, exp_total = 12, label = FALSE) +
  ggplot2::scale_fill_viridis_c(direction = -1) +
  scale_x_discrete(breaks = seq(min(data$year), max(data$year), by = 5))

# Make gaps explicit

data |>
  tsibble::as_tsibble(key = well_id, index = date) |>
  tsibble::fill_gaps()
  diff(1:3, 1)
