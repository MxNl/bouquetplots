# c(
#   "dplyr",
#   "ggplot2",
#   "purrr",
#   "readr",
#   "magrittr"
#   ) |> purrr::map(usethis::use_package)

import::from(BBmisc, normalize)
import::from(zoo, na.approx)
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

data |>
  dplyr::mutate(month_day = as.character(lubridate::day(date))) |>
  ggplot(aes(month_day)) +
  geom_histogram(stat = "count", fill = "lightblue", colour = "white") +
  theme_minimal()

# Make gaps explicit
data <- data |>
  tsibble::as_tsibble(key = well_id, index = date) |>
  tsibble::fill_gaps() |>
  dplyr::filter(lubridate::day(date) == 15) |>
  dplyr::as_tibble()

# Interpolate gaps
data <- data |>
  dplyr::group_by(well_id) |>
  dplyr::mutate(gwl = na.approx(gwl)) |>
  dplyr::ungroup()

# Prepare for bouquetplot
data <- data |>
  dplyr::group_by(well_id, year) |>
  dplyr::filter(year >= 1994) |>
  dplyr::summarise(gwl = mean(gwl)) |>
  dplyr::mutate(date_n = 1:dplyr::n(), .before = year) |>
  dplyr::ungroup()

data |>
  ggplot(aes(date_n, gwl, colour = well_id, group = well_id)) +
  geom_line()


# dplyr::tibble(well_id = "BB_testest",
#        gwl = c(1:4, 4.5, 5, 5:3, 3:7)) |>
#   dplyr::mutate(date_n = dplyr::row_number(), .before = gwl) |>
data |>
  dplyr::group_by(well_id) |>
  dplyr::mutate(
    gwl = gwl - dplyr::first(gwl),
    gwl_n = gwl - dplyr::lag(gwl), .before = gwl,
    gwl_n = tidyr::replace_na(gwl_n, 0)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(gwl_n)) |>
  dplyr::mutate(increase_check = gwl_n >= 0) |>
  dplyr::mutate(gwl_n = normalize(abs(gwl_n), method = "range")) |>
  dplyr::mutate(gwl_n = dplyr::case_when(increase_check ~ gwl_n,
                                         !increase_check ~ gwl_n * (-1))) |>
  dplyr::group_by(well_id) |>
  # dplyr::mutate(gwl_n = cumsum(gwl_n)) |>
  dplyr::mutate(gwl_n = sin(gwl_n)) |>
  ggplot(aes(date_n, gwl_n, colour = increase_check, group = well_id, label = gwl)) +
  geom_line() +
  # geom_text(nudge_y = .06) +
  # geom_point() +
  # theme_minimal() +
  theme_void() +
  coord_flip() +
  facet_wrap(~ well_id, nrow = 1)

data |>
  dplyr::group_by(well_id) |>
  dplyr::mutate(
    gwl = gwl - dplyr::first(gwl),
    gwl_n = gwl - dplyr::lag(gwl), .before = gwl,
    gwl_n = tidyr::replace_na(gwl_n, 0)) |>
  # dplyr::ungroup() |>
  dplyr::filter(!is.na(gwl_n)) |>
  dplyr::mutate(increase_check = gwl_n >= 0) |>
  dplyr::mutate(gwl_trend = dplyr::case_when(increase_check ~ 1,
                                         !increase_check ~ -1)) |>
  dplyr::mutate(gwl_trend = cumsum(gwl_trend)) |>
  dplyr::mutate(gwl_trend = dplyr::case_when(increase_check ~ gwl_trend,
                                         !increase_check ~ gwl_trend * (-1))) |>
  dplyr::mutate(gwl_trend = normalize(abs(gwl_trend), method = "range")) |>
  dplyr::mutate(gwl_trend = tan(gwl_trend)) |>
  # dplyr::select(well_id, date_n, gwl, gwl_trend) |>
  # tidyr::pivot_wider(id_cols = c("date_n"),
  #                    names_from = well_id,
  #                    values_from = c("gwl_trend", "gwl")) |>
  # head(20) |> View()
  ggplot(aes(date_n, gwl_trend, colour = increase_check, group = well_id, label = stringr::str_glue("{round(gwl, 2)}\n{round(gwl_trend, 2)}"))) +
  geom_line() +
  geom_text(nudge_y = .25, size = 2, colour = "grey30") +
  geom_point() +
  # theme_minimal() +
  theme_void() +
  coord_flip() +
  facet_wrap(~ well_id, nrow = 1)
