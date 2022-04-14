# c(
#   "dplyr",
#   "ggplot2",
#   "purrr",
#   "readr",
#   "magrittr"
#   ) |> purrr::map(usethis::use_package)

import::from(BBmisc, normalize)
import::from(zoo, na.approx)
library(dplyr)
library(ggplot2)

well_samples <- 50


# Import
set.seed(123)
data <-
  "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/Einzelmessstellen" |>
    list.files(pattern = ".txt", full.names = TRUE) |>
    sample(well_samples) |>
    purrr::map_dfr(readr::read_csv2,
                   col_types = "ciiciicdd",
                   locale = readr::locale(decimal_mark = ".", grouping_mark = ",")) |>
  janitor::clean_names() |>
  select(mest_id, datum, gw_nn) |>
  rename(well_id = mest_id,
                date = datum,
                gwl = gw_nn) |>
  mutate(date = lubridate::dmy_hms(date),
                date = as.Date(date),
                gwl = as.numeric(gwl)) |>
  mutate(year = lubridate::year(date), .after = date) |>
  group_by(well_id) |>
  arrange(date) |>
  ungroup() |>
  tidyr::drop_na(gwl)

# Meta data
data_meta <- "J:/PROJEKTE/KliBiW7/Daten/Grundwasserstandsdaten/PROJEKT_BASISDATEN.txt" |>
  readr::read_csv2() |>
  janitor::clean_names() |>
  rename(well_id = mest_id) |>
  mutate(well_id = as.character(well_id))

# Check temporal resolution
data |>
  group_by(well_id) |>
  mutate(timediff = date - lag(date),
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
  mutate(month_day = as.character(lubridate::day(date))) |>
  ggplot(aes(month_day)) +
  geom_histogram(stat = "count", fill = "lightblue", colour = "white") +
  theme_minimal()

# Make gaps explicit
data <- data |>
  tsibble::as_tsibble(key = well_id, index = date) |>
  tsibble::fill_gaps() |>
  filter(lubridate::day(date) == 15) |>
  as_tibble()

# Interpolate gaps
data <- data |>
  group_by(well_id) |>
  mutate(gwl = na.approx(gwl)) |>
  ungroup()

# Prepare for bouquetplot
data <- data |>
  group_by(well_id) |>
  # group_by(well_id, year) |>
  # filter(year >= 1994) |>
  # summarise(gwl = mean(gwl)) |>
  mutate(date_n = 1:n(), .before = year) |>
  ungroup()

data |>
  ggplot(aes(date_n, gwl, colour = well_id, group = well_id)) +
  geom_line()


data_prep <- data |>
  # tibble(well_id = "BB_testest",
  #      gwl = c(1:4, 4.5, 5, 5:3, 3:7)) |>
  # mutate(date_n = row_number(), .before = gwl) |>
  group_by(well_id) |>
  mutate(
    gwl = gwl - first(gwl),
    gwl_n = gwl - lag(gwl), .before = gwl,
    gwl_n = tidyr::replace_na(gwl_n, 0)) |>
  # ungroup() |>
  filter(!is.na(gwl_n)) |>
  mutate(increase_check = gwl_n >= 0) |>
  mutate(gwl_n = normalize(abs(gwl_n), method = "range")) |>
  mutate(gwl_trend = case_when(increase_check ~ 1,
                                             !increase_check ~ -1)) |>
  mutate(gwl_n = case_when(increase_check ~ gwl_n,
                                         !increase_check ~ gwl_n * (-1))) |>
  select(-gwl, -gwl_n) |>
  mutate(gwl_trend = if_else(date_n %in% 1:2, 0, gwl_trend)) |>
  mutate(gwl_trend_cum = cumsum(gwl_trend)) |>
  mutate(gwl_trend_cum = gwl_trend_cum)

# alpha_factor <- data_prep |>
#   ungroup() |>
#   filter(gwl_trend  != 0) |>
#   pull(gwl_trend) |>
#   rle() |>
#   purrr::chuck("lengths") |>
#   max()

alpha_factor <- data_prep |>
  ungroup() |>
  pull(gwl_trend_cum) |>
  abs() |>
  max()

{data_prep |>
  mutate(alpha = (sin(pi / 2 / alpha_factor) * gwl_trend_cum)) |>
  mutate(date_trans = cumsum(abs(cos(alpha))),
                gwl_trans = cumsum(sin(alpha))) |>
  ggplot(aes(date_trans, gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}"))) +
  geom_line() +
  # geom_text(nudge_y = 10, size = 2, colour = "grey30") +
  geom_point(aes(colour = increase_check)) +
  theme_void() +
  coord_flip()} |> plotly::ggplotly()
  # facet_wrap(~ well_id, nrow = 1)

data_prep <- data_prep |>
  mutate(alpha = (sin(pi / 2 / alpha_factor) * gwl_trend_cum)) |>
  mutate(date_trans = cumsum(abs(cos(alpha))),
                gwl_trans = cumsum(sin(alpha)))

data_prep |>
  ggplot(aes(date_trans, gwl_trans, colour = gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}"))) +
  geom_line(show.legend = FALSE) +
  geom_point(data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  scale_colour_viridis_c() +
  # geom_text(nudge_y = 10, size = 2, colour = "grey30") +
  # geom_point() +
  theme_void() +
  coord_flip()

data_prep |>
  ggplot(aes(date_trans, gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}"))) +
  geom_line(colour = "darkgreen", show.legend = FALSE, lineend = "round") +
  theme_void() +
  coord_flip()
