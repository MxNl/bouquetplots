# https://zenodo.org/record/4683879
library(ggplot2)


files <- "data/GWData/" |> list.files(full.names = TRUE)

data_gw <- files |> purrr::map(readr::read_csv)

well_ids <- files |>
  basename() |>
  stringr::str_remove("_GW-Data.csv")

data_gw <- data_gw |>
  purrr::map2_dfr(well_ids, ~ .x |> dplyr::mutate(well_id = .y, .before = 1))


data_gw <- data_gw |>
  janitor::clean_names() |>
  dplyr::mutate(date = lubridate::dmy_hms(date))

data_gw |>
  dplyr::group_by(well_id) |>
  dplyr::summarise(date_min = min(date),
                   date_max = max (date)) |>
  ggplot(aes(xmin = as.Date(date_min), xmax = as.Date(date_max), y = well_id)) +
  geom_linerange()

library(showtext)

font_add("Fira Code", "ttf/FiraCode-Regular.ttf", "ttf/FiraCode-Bold.ttf")
# font_families()
showtext_auto()

data_climate_spiral <- data_gw |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date),
         .after = date) |>
  filter(year >= 1980 & year <= 2019) |>
  group_by(well_id) |>
  # mutate(gwl = normalize(gwl, method = "range")) |>
  mutate(gwl = gwl - first(gwl)) |>
  group_by(year, month, week) |>
  summarise(gwl = mean(gwl)) |>
  ungroup() |>
  filter(week != 53)

data_climate_spiral <- data_climate_spiral |>
  group_by(year) |>
  tidyr::complete(week = seq(0, 52, 1)) |>
  ungroup() |>
  tidyr::fill(gwl, month, .direction = "down") |>
  arrange(year, month, week) |>
  tidyr::drop_na() |>
  # 2 data points in first frame for geom_line to work
  mutate(frame = lag(row_number())) |>
  tidyr::fill(frame, .direction = "up") |>
  mutate(year_week = year + (week - 1) * 1 / 52,
         year = as.character(year)) |>
  mutate(gwl_stripes = mean(gwl) - gwl) |>
  relocate(year_week, year, month, week, frame, gwl, gwl_stripes) |>
  mutate(date = lubridate::ym(paste(year, "-", month))) |>
  group_by(year, month) |>
  mutate(date = date + row_number())

p <- data_climate_spiral |>
  ggplot(aes(x = date, y = gwl_stripes, colour = as.numeric(year), group = year)) +
  geom_line(show.legend = TRUE) +
  scale_colour_viridis_c() +
  ggdark::dark_mode(theme_minimal()) +
  coord_polar(theta = "x") +
  scale_x_continuous(
    breaks = seq(1, 12, 1),
    labels = month.abb
  ) +
  # scale_y_continuous(limits = c(0, 1)) +
  # ggtitle("asd") +
  theme(plot.title = element_text(hjust = .43, vjust = -80, face = "bold", size = 16),
        text = element_text(family = "Fira Code"),
        axis.title.y = element_text(hjust = .72, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank())

p_anim <- p +
  gganimate::transition_reveal(year_month) +
  ggtitle("{as.character(floor(frame_along))}") +
  ease_aes("linear")

animate(p_anim, nrow(data_climate_spiral), 10, rewind = FALSE)

anim_save("gwl_spiral.gif")
