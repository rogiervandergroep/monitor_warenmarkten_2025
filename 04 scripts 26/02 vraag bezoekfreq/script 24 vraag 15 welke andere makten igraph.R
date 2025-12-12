# install.packages("igraph")
# library
library(igraph)

source(
  'http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R'
)


# winkelgebieden_kaart <- sf::read_sf(
#   "https://onderzoek.amsterdam.nl/static/datavisualisatie-onderzoek-en-statistiek/geo/winkelgebieden/2022/winkelgebieden-2022-geo.json"
# )

source(
  "04 scripts 26/02 vraag bezoekfreq/script 24 vraag 14 welke andere makten.R"
)


# create data:

df_markten <- df_v14a_anderemarkten_selectie |>
  filter(
    v14 != 'geen andere markten',
    v14 != 'ik kan de markt niet vinden in deze lijst'
  ) |>
  rename(
    target = v14,
    source = markt,
    weight = aandeel
  ) |>
  mutate(
    type_markt2 = factor(
      type_markt2,
      levels = c("markt op meerdere dagen", "eendaagse markt")
    )
  )


df_meta <- df_markten |>
  select(source, type_markt2, stadsdeel_markt) |>
  distinct()


# Turn it into igraph object
network_markt <- graph_from_data_frame(
  d = df_markten,
  vertices = df_meta,
  directed = T
)

# Create a vector of color
blauw_pal <- c(
  "#004699",
  "#3858a4",
  "#566bb0",
  "#707ebb",
  "#8992c6",
  "#a1a7d2",
  "#b8bcdd",
  "#d0d2e8",
  "#e7e8f4"
)

discreet <- c(
  "#6cbd74",
  "#ff9100"
)

# Create a vector of color
kleur_eendag_meerdag <- discreet[as.numeric(as.factor(
  V(network_markt)$type_markt2
))]

grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"

# gewicht
deg_w <- strength(network_markt, weights = E(network_markt)$weight)
V(network_markt)$size <- deg_w * 10 + 10

# aantal pijlen
# deg_c <- degree(network_markt)
# V(network_markt)$size <- deg*2+3  # scale so they are visible

svg("06 output figuren/testplot.svg", width = 12, height = 7)

# Plot the customized graph
plot(
  network_markt,
  edge.width = E(network_markt)$weight * 30 + 2, # Edge width based on weight
  vertex.label.family = font,
  vertex.frame.color = kleur_eendag_meerdag,
  vertex.color = kleur_eendag_meerdag,
  vertex.label.color = "black",
  edge.color = blauw_pal[5]
)

# Add a legend
legend(
  "bottomleft",
  legend = levels(as.factor(V(network_markt)$type_markt2)),
  col = kleur_eendag_meerdag,
  bty = "n",
  pch = 20,
  pt.cex = 3,
  cex = 1.5,
  text.col = kleur_eendag_meerdag,
  horiz = F,
  inset = c(0.1, 0.1)
)


dev.off()
