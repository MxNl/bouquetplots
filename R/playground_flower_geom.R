geom_rose <- function(n, mean = c(0, 0), ...) {
  .x <- mvtnorm::rmvnorm(n, mean)
  df <- tibble(x = .x[, 1], y = .x[, 2])

  list(
    stat_density_2d(
      aes(x = x, y = y, fill = ..level..),
      data = df,
      geom = "polygon", show.legend = FALSE, color = NA
    ),
    scale_fill_gradient2(...)
  )
}

subset <- data_prep |>
  filter(date_n == max(date_n)) |>
  ungroup() |>
  slice(1) |>
  select(contains("trans")) |>
  unlist()

data_prep |>
  ggplot() +
  geom_line(
    aes(
      date_trans, gwl_trans,
      colour = gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}")
    ),
    show.legend = FALSE
  ) +
  geom_point(aes(
    date_trans, gwl_trans,
    colour = gwl_trans, group = well_id, label = stringr::str_glue("{round(gwl_trans)}\n{round(gwl_trend, 2)}")
  ),
  data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  geom_rose(1000, mean = subset,
            low = "red", mid = "purple", high = "pink",
            midpoint = 0.075) +
  scale_colour_viridis_c() +
  # geom_text(nudge_y = 10, size = 2, colour = "grey30") +
  # geom_point() +
  theme_void() +
  coord_flip()




p <- ggplot() +
  coord_equal(1, c(-4, 2), c(-7, 3)) +
  geom_curve(aes(x = -1, y = -7, xend = 0, yend = 0),
             ncp = 1000, curvature = -0.3, size = 1,
             color = "olivedrab3")
p

geom_leaf <- function(x, xend, f, xoffset = 0, yoffset = 0,
                      xflip = 1, yflip = 1, ...) {

  .x <- seq(x, xend, length.out = 100)
  .y <- f(.x)

  df <- tibble(x = c(.x, .y), y = c(.y, .x))
  df$x <- xflip * df$x + xoffset
  df$y <- yflip * df$y + yoffset

  geom_polygon(aes(x = x, y = y), data = df, ...)
}

f <- function(x) x^2 / 2

p <- p +
  geom_leaf(0, 2, f, -1.6, -4.5, 1,
            fill = "olivedrab3", color = "palegreen") +
  geom_leaf(0, 2, f, -1.6, -5,  -1,
            fill = "olivedrab3", color = "palegreen")
p

geom_rose <- function(n, mean = c(0, 0), ...) {

  .x <- mvtnorm::rmvnorm(n, mean)
  df <- tibble(x = .x[, 1], y = .x[, 2])

  list(
    stat_density_2d(
      aes(x = x, y = y, fill = ..level..), data = df,
      geom = "polygon", show.legend = FALSE, color = "grey80"),
    scale_fill_gradient2(...)
  )
}

p +
  geom_rose(1000, mean = c(0, 0),
            low = "red", mid = "purple", high = "pink",
            midpoint = 0.075)

