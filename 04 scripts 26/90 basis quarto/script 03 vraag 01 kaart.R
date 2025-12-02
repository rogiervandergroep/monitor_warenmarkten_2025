###

# install.packages("tidyverse")
# install.packages("sf")
# install.packages("ggspatial")
# install.packages("raster")
# install.packages("prettymapr")
# install.packages("openxlsx")
install.packages("knitr")
install.packages("svglite")


library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(prettymapr)
library(svglite)


source(
  "https://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/OS_ggtheme.R"
)


winkelgebieden_kaart <- sf::read_sf(
  "https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/winkelgebieden/2022/winkelgebieden-2022-geo.json"
)


# Make sure it's in WGS84 (lat/lon), which leaflet expects
wink_amers <- st_transform(winkelgebieden_kaart, 4326)

kaart <- wink_amers |>
  filter(naam == "Albert Cuypstraat")

kaart_centroid <- st_centroid(kaart) |>
  mutate(
    longtitude = st_coordinates(geometry)[, 1],
    latitude = st_coordinates(geometry)[, 2]
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


url1 = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/${z}/${x}/${y}.png"

kaart_def <- kaart_centroid |>
  ggplot() +
  annotation_map_tile(type = url1, zoomin = 0) +
  geom_sf(color = NA, fill = NA) +
  geom_sf(data = kaart, color = 'black', fill = alpha("#5167ad", 0.2)) +
  theme_os_map()


write_rds(kaart_def, "07 QUARTO/kaart_cuyp.rds")
