timeseries_test <- tibble(
  trend = c(0, 0, 1, -1, 1, 1, 1, 1, 1, 1, 1, -1, -1, 1, -1),
  gwl_trend_cum = cumsum(trend)
)

max_cum_steps <- timeseries_test |>
  pull(gwl_trend_cum) |>
  abs() |>
  max()

angle_step <- sin(pi * 2 / max_cum_steps)



# Trial 1 -----------------------------------------------------------------
timeseries_test_trans <- timeseries_test |>
  mutate(alpha = angle_step * gwl_trend_cum) |>
  mutate(
    date_trans = cumsum(abs(cos(alpha))),
    gwl_trans = cumsum(sin(alpha))
  )

timeseries_test_trans |>
  ggplot(aes(date_trans, gwl_trans, colour = gwl_trans, label = trend)) +
  geom_line(show.legend = FALSE) +
  # geom_point(data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  scale_colour_viridis_c() +
  geom_label() +
  # geom_point() +
  theme_void() +
  coord_flip()

# Trial 2 -----------------------------------------------------------------
timeseries_test_trans <- timeseries_test |>
  mutate(alpha = angle_step * gwl_trend_cum) |>
  mutate(
    date_trans = cos(alpha),
    gwl_trans = sin(alpha)
  )

timeseries_test_trans |>
  ggplot(aes(date_trans, gwl_trans, colour = gwl_trans, label = trend)) +
  geom_line(show.legend = FALSE) +
  # geom_point(data = data_prep |> filter(date_n == max(date_n)), show.legend = FALSE) +
  scale_colour_viridis_c() +
  geom_label() +
  # geom_point() +
  theme_void() +
  coord_flip()
