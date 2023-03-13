library(tidyverse)
library(spotifyr)
library(compmus)

Topsongsdrake <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "7dl2AkcjDVWdVxwCH2QAWV"
  ) |>
  slice(1:30) |>
  add_audio_analysis()

Rapcaviar <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "37i9dQZF1DX0XUsuxWHRQd"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
CompareHiphop <-
  Topsongsdrake |>
  mutate(genre = "Topsongsdrake") |>
  bind_rows(bigband |> mutate(genre = "Rapcaviar"))

CompareHiphop |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )

