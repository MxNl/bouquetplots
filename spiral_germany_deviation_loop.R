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
  mutate(size = if_else(month >= 13, 0, 4))

# loop --------------------------------------------------------------------

germany <- rnaturalearth::ne_countries(
  scale = 50,
  returnclass = "sf",
  sovereignty = "Germany") |>
  select(geometry)

inset_germany <-
  germany |>
  ggplot() +
  geom_sf(colour = NA, fill = "grey30") +
  theme_void() +
  theme(plot.margin = unit(rep(.0,4), "cm"))
# ggdark::dark_mode(theme_void())


draw_spiral <- function(i = 1, inset_plot) {
  # plot sequence
  data <- data_climate_spiral |>
    filter(frame <= i)

  p <- data |>
    ggplot(aes(x = month, y = gwl_stripes, colour = gwl_stripes)) +
    geom_line(aes(group = year
                  # size = year_month |> normalize("range", range = c(.5,2))
    ),
    show.legend = FALSE) +
    ggfx::with_shadow(geom_point(data = data |> slice_tail(n = 1),
                                 aes(group = year),
                                 size = 6,
                                 show.legend = FALSE),x_offset = 0, y_offset = 0, sigma = 5,
                      colour = "black") +
    scico::scale_colour_scico(palette = "roma", direction = -1) +
    scico::scale_fill_scico(palette = "roma", direction = -1) +
    # scale_fill_gradient2(low = "blue",
    #                      mid = "white",
    #                      high = "red") +
    # scale_colour_gradient2(low = "blue",
    #                        mid = "white",
    #                        high = "red") +
    scale_size_identity() +
    # scale_fill_viridis_c() +
    # scale_colour_viridis_c() +
    ggdark::dark_mode(theme_minimal()) +
    coord_polar(theta = "x") +
    scale_x_continuous(
      breaks = seq(1, 12, 1),
      labels = month.abb
    ) +
    scale_y_continuous(limits = range(data_climate_spiral$gwl_stripes)) +
    ggtitle(data_climate_spiral |> filter(frame == i) |> pull(year) |> unique()) +
    labs(subtitle = "Groundwater level deviation\nfrom all time mean in meters",
         y = "") +
    theme(plot.title = element_text(hjust = .43, vjust = -62, face = "bold", size = 16),
          plot.subtitle = element_text(lineheight = 2),
          text = element_text(family = "Fira Code"),
          axis.title.y = element_text(hjust = .72, size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_blank(),
          # plot.margin = unit(rep(.0,4), "cm"),
          plot.background=element_rect(colour=NA))
  # plot.background=element_rect(fill=NA, colour=NA))

  p <- p + patchwork::inset_element(inset_plot, left = -0.1, bottom = .8, right = .3, top = 1)
  # inset_germany + patchwork::inset_element(p, left = 0, bottom = .2, right = .9, top = .9)

  print(p)
}

tictoc::tic()
animation::saveGIF({
  for (i in 485:(nrow(data_climate_spiral)-1)) {
    draw_spiral(i, inset_germany)
  }},
  movie.name = "gwl_spiral_loop_test.gif",
  interval = 1/12,
  ani.width = 600,
  ani.height = 600,
  ani.res = 50
)
tictoc::toc()

magick::image_read("gwl_spiral_loop_test.gif") |>
  magick::image_trim() |>
  magick::image_write("gwl_spiral_loop_test_trim.gif")

tictoc::tic()
animation::saveGIF({
  for (i in 1:(nrow(data_climate_spiral)-1)) {
    draw_spiral(i, inset_germany)
  }},
  movie.name = "gwl_spiral_loop.gif",
  interval = 1/12,
  ani.width = 600,
  ani.height = 600,
  ani.options = c(loop = FALSE)
)
tictoc::toc()

magick::image_read("gwl_spiral_loop.gif") |>
  magick::image_trim() |>
  magick::image_write("gwl_spiral_loop_trim.gif")

