library(gganimate)
library(showtext)

font_add("Fira Code", "ttf/FiraCode-Regular.ttf", "ttf/FiraCode-Bold.ttf")
# font_families()
showtext_auto()

data_climate_spiral <- data |>
  filter(year >= 1980) |>
  group_by(well_id, year) |>
  mutate(count = n()) |>
  filter(count == 12) |>
  ungroup() |>
  inner_join(data_meta |> select(well_id, gok_nn)) |>
  mutate(gwl = gok_nn - gwl) |>
  # group_by(well_id) |>
  # mutate(gwl = normalize(gwl, method = "range")) |>
  # mutate(gwl = gwl - first(gwl)) |>
  group_by(year = lubridate::year(date), month = lubridate::month(date)) |>
  summarise(gwl = mean(gwl))

data_climate_spiral <- data_climate_spiral |>
  group_by(year) |>
  tidyr::complete(month = seq(0, 12, 1)) |>
  ungroup() |>
  tidyr::fill(gwl, .direction = "down") |> View()
  arrange(year, month) |>
  tidyr::drop_na() |>
  # 2 data points in first frame for geom_line to work
  mutate(frame = lag(row_number())) |>
  tidyr::fill(frame, .direction = "up") |>
  mutate(month = stringr::str_pad(month, width = 2, pad = "0", side = "left")) |>
  mutate(year_month = as.numeric(paste0(year, ".", month)),
         month = as.numeric(month),
         year = as.character(year))


# gganimate ---------------------------------------------------------------
p <- data_climate_spiral |>
  ggplot(aes(x = month, y = gwl, colour = gwl)) +
  geom_line(aes(group = year), show.legend = FALSE) +
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



# loop --------------------------------------------------------------------
draw_spiral <- function(i = 1) {
  # plot sequence
  data <- data_climate_spiral |>
    filter(frame <= i)

  p <- data |>
    ggplot(aes(x = month, y = gwl, colour = gwl)) +
    geom_line(aes(group = year),
              show.legend = FALSE) +
    ggfx::with_shadow(geom_point(data = data |> slice_tail(n = 1),
               aes(group = year),
               size = 4,
               show.legend = FALSE),x_offset = 0, y_offset = 0, sigma = 5,
               colour = "white") +
    scale_fill_viridis_c() +
    scale_colour_viridis_c() +
    ggdark::dark_mode(theme_minimal()) +
    coord_polar(theta = "x") +
    scale_x_continuous(
      breaks = seq(1, 12, 1),
      labels = month.abb
    ) +
    scale_y_continuous(limits = range(data_climate_spiral$gwl)) +
    ggtitle(data_climate_spiral |> filter(frame == i) |> pull(year) |> unique()) +
    theme(plot.title = element_text(hjust = .43, vjust = -62, face = "bold", size = 16),
          text = element_text(family = "Fira Code"),
          axis.title.y = element_text(hjust = .72, size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.x = element_blank())

  print(p)
}

animation::saveGIF({
  for (i in 520:(nrow(data_climate_spiral)-1)) {
    draw_spiral(i)
  }},
  movie.name = "gwl_spiral_loop_test.gif",
  interval = 1/12,
  ani.width = 600,
  ani.height = 600,
  ani.res = 50
)

magick::image_read("gwl_spiral_loop_test.gif") |>
  magick::image_trim() |>
  magick::image_write("gwl_spiral_loop_test_trim.gif")

animation::saveGIF({
  for (i in 1:(nrow(data_climate_spiral)-1)) {
    draw_spiral(i)
  }},
  movie.name = "gwl_spiral_loop.gif",
  interval = 1/12,
  ani.width = 600,
  ani.height = 600,
  ani.options = c(loop = FALSE)
)

magick::image_read("gwl_spiral_loop.gif") |>
  magick::image_trim() |>
  magick::image_write("gwl_spiral_loop_trim.gif")
