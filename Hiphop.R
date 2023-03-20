library(tidyverse)
library(spotifyr)
library(compmus)

DrakeTop <-
  get_playlist_audio_features(
    "",
    "7dl2AkcjDVWdVxwCH2QAWV"
  ) |>
  slice(1:25) |>
  add_audio_analysis()
RapCaviar <-
  get_playlist_audio_features(
    "",
    "37i9dQZF1DX0XUsuxWHRQd"
  ) |>
  slice(1:25) |>
  add_audio_analysis()
Popchart <-
  get_playlist_audio_features(
    "",
    "1eKpGEx2H4cB6DBs34obeR"
  ) |>
  slice(1:25) |>
  add_audio_analysis()

DrakeCaviar <-
  DrakeTop |>
  mutate(Category = "Drake Top Songs") |>
  bind_rows(RapCaviar |> mutate(Category = "Hip hop Charts"), Popchart |> mutate(Category = "Pop Charts"))


DrakeCaviar |>
  mutate(
    sections =
      map(
        segments,                                    # sections or segments
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
      colour = Category,
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
    colour = "Category",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )
