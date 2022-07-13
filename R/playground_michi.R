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
  mutate(date_n = 1:n(), .before = year) |>
  ungroup()

data_prep <- data |>
  group_by(well_id) |>
  mutate(
    gwl = gwl - first(gwl),
    gwl_n = gwl - lag(gwl), .before = gwl,
    gwl_n = tidyr::replace_na(gwl_n, 0)) |>
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

max_cum_steps <- data_prep |>
  ungroup() |>
  pull(gwl_trend_cum) |>
  abs() |>
  max()

# Versuch 1 ---------------------------------------------------------------
angle_step <- sin(pi / 2 / max_cum_steps)

data_prep <- data_prep |>
  mutate(angle_step = angle_step * gwl_trend_cum) |>
  mutate(
    date_trans = cumsum(abs(cos(angle_step))),
    gwl_trans = cumsum(sin(angle_step))
  )

data_prep |>
  ggplot(aes(date_trans, gwl_trans, colour = gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}"))) +
  geom_line(show.legend = FALSE) +
  geom_point(data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  scale_colour_viridis_c() +
  # geom_text(nudge_y = 10, size = 2, colour = "grey30") +
  # geom_point() +
  theme_void() +
  coord_flip()

# Versuch 2 ---------------------------------------------------------------
angle_step <- sin(pi * 2 / max_cum_steps)

data_prep <- data_prep |>
  mutate(angle_step = angle_step * gwl_trend_cum) |>
  mutate(
    date_trans = cos(angle_step),
    gwl_trans = sin(angle_step)
  )

data_prep |>
  ggplot(aes(date_trans, gwl_trans, colour = gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}"))) +
  geom_line(show.legend = FALSE) +
  geom_point(data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  scale_colour_viridis_c() +
  # geom_text(nudge_y = 10, size = 2, colour = "grey30") +
  # geom_point() +
  theme_void() +
  coord_flip()

# Versuch 3 ---------------------------------------------------------------
data_prep <-
  data_prep |>
  # mutate(alpha = (sin(pi * 2 / alpha_factor) * gwl_trend_cum)) |>
  mutate(angle_step = (pi * 2 / max_cum_steps) * gwl_trend_cum) |>
  mutate(
    date_trans = cumsum(angle_step%%(max_cum_steps)),
    gwl_trans = cumsum(angle_step)
  )

data_prep |>
  ggplot(aes(date_trans, gwl_trans, colour = gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}"))) +
  geom_line(show.legend = FALSE) +
  geom_point(data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  scale_colour_viridis_c() +
  # geom_text(nudge_y = 10, size = 2, colour = "grey30") +
  # geom_point() +
  theme_void() +
  coord_flip()
