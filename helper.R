Passionfruit <-
  get_tidy_audio_analysis("5mCPDVBb16L4XQwDdbRUpz") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

Passionfruit_plot <- Passionfruit |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() + ggtitle("Passionfruit - chromagram") +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

saveRDS(object = Passionfruit_plot, file = "data/grav-plot-week-11.RDS")