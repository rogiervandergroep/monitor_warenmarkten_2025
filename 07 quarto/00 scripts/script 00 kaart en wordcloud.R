source("07 quarto/00 scripts/script 00 plot functies.R")
library(tidyverse)

library(ggspatial)

markten_kaart <- sf::read_sf(
  "https://maps.amsterdam.nl/open_geodata/geojson_lnglat.php?KAARTLAAG=MARKTEN&THEMA=markten"
)

markten_kaart <- markten_kaart |>
  mutate(
    Locatie = case_when(
      Locatie == 'Plein `40-`45' ~ "Plein 40-45",
      Locatie == "Waterlooplein" ~ "Waterloopleinmarkt",
      Locatie == "Tussen Meer" ~ "Tussenmeer",
      Locatie == "Van Eesterenlaan" ~ "Biomarkt Zeeburg",
      Locatie == "Stadionplein" ~ "Stadionpleinmarkt",
      TRUE ~ Locatie
    )
  ) |>
  filter(Locatie %in% levels_markt) |>
  filter(Locatie != 'Noordermarkt' | SELECTIE == 'ALGEMEEN') |>
  mutate(
    longtitude = st_coordinates(geometry)[, 1],
    latitude = st_coordinates(geometry)[, 2]
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


st_write(
  markten_kaart,
  "07 quarto/01 intermediate/kaart_basis.csv"
)

#   "Weesp"
# 52.306608, 5.042417

functie_kaart <- function(x) {
  kaart_centroid <- st_centroid(x) |>
    mutate(
      longtitude = st_coordinates(geometry)[, 1],
      latitude = st_coordinates(geometry)[, 2]
    ) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  url1 = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/${z}/${x}/${y}.png"

  kaart_def <- kaart_centroid |>
    ggplot() +
    annotation_map_tile(type = url1, zoom = 20) +
    geom_sf(color = NA, fill = NA) +
    geom_sf(data = x, color = 'black', fill = alpha("#5167ad", 0.2)) +
    theme_os_map()
}


#write_rds(functie_kaart, "07 quarto/01 intermediate/functie_kaart.rds")

#markten_kaart <- read_rds("01 intermediate/kaart_basis.rds")
#functie_kaart <- read_rds("01 intermediate/functie_kaart.rds")

kaart <- markten_kaart |>
  filter(Locatie == "Buikslotermeerplein") |>
  functie_kaart()

plot(kaart)

#install.packages("wordcloud2")
#install.packages("webshot2")
#install.packages("htmlwidgets")

anno_df_som <- read_rds(
  "07 quarto/01 intermediate/tabel_v0_steekwoorden.rds"
) |>
  filter(lemma != 'allochtonen')

wild <- c(
  "#a00078",
  "#e50082",
  "#009dec",
  "#fb9bbe",
  "#d48fb9",
  "#a4ccf3",
  "#ffd8e5",
  "#efd2e3",
  "#dceafa"
)

wordcloudfunctie <- function(df, m) {
  wc <- df |>
    ungroup() |>
    filter(markt == m) |>
    slice_max(tf_idf, n = 25, with_ties = T) |>
    dplyr::select(lemma, aantal) |>
    wordcloud2::wordcloud2(fontFamily = font, color = wild)

  htmlwidgets::saveWidget(
    wc,
    "07 quarto/01 intermediate/temp.html",
    selfcontained = TRUE
  )
  webshot2::webshot(
    "07 quarto/01 intermediate/temp.html",
    glue::glue("07 quarto/02 figuren/wordcloud_{ m }.png"),
    zoom = 2
  )
}


levels_markt |>
  map(\(x) wordcloudfunctie(anno_df_som, m = x))
