library(readr)
library(dplyr)
library(ggplot2)


wine <- read_csv("data/winemag-data_first150k.csv", show_col_types = FALSE)

# Erstelle eine Liste der Länder für Länderauswahl
list_countries <- wine |>
  filter(!is.na(country)) |>
  distinct(country) |>
  arrange(country) |>
  pull(country)
