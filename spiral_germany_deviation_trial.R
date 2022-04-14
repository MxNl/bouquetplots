# https://zenodo.org/record/4683879
library(ggplot2)
library(dplyr)
library(rnaturalearth)


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
font_add("Wakaba", "ttf/Wakaba (Demo Version).ttf")
# font_families()
showtext_auto()

data_climate_spiral <- data_gw |>
  mutate(year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date),
         .after = date) |>
  filter(year >= 1980 & year <= 2019) |>
  group_by(well_id) |>
  mutate(gwl = gwl - first(gwl)) |>
  mutate(gwl = BBmisc::normalize(gwl, method = "range")) |>
  group_by(year, month) |>
  summarise(gwl = mean(gwl)) |>
  ungroup()

data_climate_spiral <- data_climate_spiral |>
  group_by(year) |>
  tidyr::complete(month = seq(0, 12, 1)) |>
  ungroup() |>
  tidyr::fill(gwl, .direction = "down") |>
  arrange(year, month) |>
  tidyr::drop_na() |>
  # 2 data points in first frame for geom_line to work
  arrange(year, month) |>
  tidyr::drop_na() |>
  # 2 data points in first frame for geom_line to work
  # mutate(frame = lag(row_number())) |>
  # tidyr::fill(frame, .direction = "up") |>
  mutate(frame = 1:n()) |>
  mutate(gwl_stripes = mean(gwl) - gwl) |>
  # mutate(month = stringr::str_pad(month, width = 2, pad = "0", side = "left")) |>
  mutate(year_month = year + month * 1 / 12,
         month = as.numeric(month),
         year = as.character(year)) |>
  mutate(size = if_else(month >= 12, 0, 4))

# gganimate ---------------------------------------------------------------
p <- data_climate_spiral |>
  ggplot(aes(x = month, y = gwl_stripes, colour = gwl_stripes, group = year)) +
  geom_line(show.legend = FALSE) +
  # gghighlight::gghighlight(year >= year_month - 2) +
  # ggnewscale::new_scale_colour() +
  ggfx::with_shadow(geom_point(
    aes(size = size, fill = gwl_stripes),
    # data = data_climate_spiral |> filter(month != 0),
    # colour = NA,
    # size = 3,
    show.legend = FALSE
  ),
  x_offset = 0, y_offset = 0, sigma = 5,
  colour = "white"
  ) +
  scale_size_identity() +
  # scico::scale_colour_scico(palette = "grayC") +
  scico::scale_colour_scico(palette = "roma", direction = -1) +
  scico::scale_fill_scico(palette = "roma", direction = -1) +
  ggdark::dark_mode(theme_minimal()) +
  coord_polar(theta = "x") +
  scale_x_continuous(
    breaks = seq(1, 12, 1),
    labels = month.abb
  ) +
  scale_y_continuous(limits = range(data_climate_spiral$gwl_stripes)) +
  # ggtitle("asd") +
  # labs(subtitle = "<span style = 'font-size:12pt;'>Groundwater Level Fluctuations</span><br><span style = 'font-size:8pt;'>as deviation from all time mean in meters</span><br>-- **Germany** --",
  #      y = "") +
  labs(
    subtitle = "Groundwater Level Fluctuations\nas deviation from all time mean in meters\n-- Germany --",
    y = ""
  ) +
  # labs(
  #   subtitle = "Groundwater Level Fluctuations",
  #   y = ""
  # ) +
  theme(
    plot.title = element_text(hjust = .43, vjust = -90, face = "bold", size = 16),
    plot.subtitle = element_text(lineheight = 1.3, hjust = .5, family = "Fira Code"),
    text = element_text(family = "Fira Code"),
    axis.title.y = element_text(hjust = .72, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title.align = 30,
    legend.title = element_text(family = "Fira Code"),
    # plot.margin = unit(rep(.0,4), "cm"),
    plot.background = element_rect(colour = NA)
  ) +
  guides(colour = guide_legend(override.aes = list(linetype = 0, shape=NA), label = FALSE))
p
# p <- p + patchwork::inset_element(inset_germany, left = .8, bottom = .9, right = 1, top = 1)

p_anim <- p +
  gganimate::transition_reveal(year_month) +
  ggtitle("{as.character(floor(frame_along))}") +
  gganimate::ease_aes("circular-in")

gganimate::animate(p_anim,
                   # 10,
                   nrow(data_climate_spiral),
                   10,
                   # end_pause = 10,
                   rewind = FALSE)

gganimate::anim_save("gwl_spiral_norm.gif")

magick::image_read("gwl_spiral_norm.gif") |>
  magick::image_trim() |>
  magick::image_write("gwl_spiral_norm_trim.gif")

