os_blauw <- c(
  "#e6e6e6",
  "#dcddee",
  "#b8bcdd",
  "#959dcc",
  "#707ebb",
  "#4861aa",
  "#004699"
)

grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"

# uit de monitor detailhandel -
det_geen <- read_rds("03 data/det_geenmrkt.rds") |>
  filter(gebied_naam == params$stadsdeel) |>
  select(aandeel_gew) |>
  pull()

det_meest <- read_rds("03 data/det_meestgen.rds") |>
  filter(gebied_naam == params$stadsdeel) |>
  select(v15_schoon) |>
  pull()

det_prijs <- read_rds("03 data/tabellen_markt_prijs.rds") |>
  filter(
    monitor == 'monitor 2026',
    markt == params$markt
  ) |>
  select(uitgaven) |>
  pull()

# open antwoorden vraag 3 rfeden bezoek

# anders
v3_anders <- read_rds("03 data/tab_v3_reden_anders.rds") |>
  filter(markt == params$markt) |>
  select(v3) |>
  pull()

# gezellig
v3_gezellig <- read_rds("03 data/tab_v3_reden_gezellig.rds") |>
  filter(markt == params$markt) |>
  select(v3) |>
  pull()

# meest gekochte product
v4_max <- read_rds("03 data/tab_v4_max.rds") |>
  filter(markt == params$markt) |>
  select(labels) |>
  pull()

# andere producten
v4_anders <- read_rds("03 data/tab_v4_andereprod.rds") |>
  filter(markt == params$markt) |>
  select(v4) |>
  pull()

tab_open <- read_rds("03 data/tab_openant.rds") |>
  map(\(x) {
    filter(x, markt == params$markt) |>
      select(vraag_open) |>
      pull()
  })


# open antwoorden bij vraag of er sprake if van toename of afname bezoekers
tab_v4b_toename <- read_rds("03 data/tab_reden_on_4b.rds") |>
  filter(
    v4a == 'toegenomen',
    markt == params$markt
  ) |>
  select(v4b_reden) |>
  pull()

tab_v4b_afname <- read_rds("03 data/tab_reden_on_4b.rds") |>
  filter(
    v4a == 'afgenomen',
    markt == params$markt
  ) |>
  select(v4b_reden) |>
  pull()

# v6 toelichting bij vraag v5: samenwerking markt
tab_v6_toelichting <- read_rds("03 data/tab_v6_ond_toel.rds") |>
  filter(markt == params$markt) |>
  select(v6_open) |>
  pull()

# sterk en zwakke punten volgens ondernemers -

tab_v11_sterk <- read_rds("03 data/tab_v11_sterk_open.rds") |>
  filter(
    markt == params$markt
  ) |>
  select(v11_open) |>
  pull()

tab_v12_zwak <- read_rds("03 data/tab_v12_zwak_open.rds") |>
  filter(
    markt == params$markt
  ) |>
  select(v12_open) |>
  pull()


### v16 toelichting of ondernemer er na tien jaar nog staat ---
tab_v16_open_ja <- read_rds("03 data/tab_v16_open.rds") |>
  filter(
    v16a == 'ja',
    markt == params$markt
  ) |>
  select(v16_open) |>
  pull()


### v16 toelichting of ondernemer er na tien jaar nog staat ---
tab_v16_open_nee <- read_rds("03 data/tab_v16_open.rds") |>
  filter(
    v16a == 'nee',
    markt == params$markt
  ) |>
  select(v16_open) |>
  pull()


### opmerkingen bezoekers en ondernemers
tab_ond_opm <- read_rds("03 data/tab_ond_opmerkingen.rds") |>
  filter(markt == params$markt) |>
  select(opmerkingen) |>
  pull()

### opmerkingen bezoekers en ondernemers
tab_bez_opm <- read_rds("03 data/tab_bez_opmerkingen.rds") |>
  filter(markt == params$markt) |>
  select(opmerkingen) |>
  pull()


resp <- read_rds("03 data/tab_respons.rds")

resp_bez <- resp |>
  filter(name == "bezoekers") |>
  filter(markt == params$markt) |>
  select(value) |>
  pull()


resp_ond <- resp |>
  filter(name == "ondernemers") |>
  filter(markt == params$markt) |>
  select(value) |>
  pull()

freq_bez <- read_rds("03 data/tab_freq.rds") |>
  filter(markt == params$markt) |>
  select(freq) |>
  pull()

# meest genoemde vervoermiddel
verv <- read_rds("03 data/tab_verv.rds") |>
  filter(achtergrond_type == params$markt) |>
  select(v6) |>
  pull()

# meest genoemde vervoermiddel
geenmarkt <- read_rds("03 data/tab_geenanderemarkt.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


meestanderemarkt <- read_rds("03 data/tab_meestgenoemdeanderemarkt.rds") |>
  filter(markt == params$markt) |>
  select(v14) |>
  pull()


v8_goed <- read_rds("03 data/v8_goed.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


### rapportcijfers ---

rap_cijfers <- read_rds("03 data/tab_rapportcijfers.rds")

rap_ond <- rap_cijfers |>
  filter(doelgroep == "ondernemers") |>
  filter(markt == params$markt) |>
  select(gemiddelde) |>
  pull()

rap_bez <- rap_cijfers |>
  filter(doelgroep == "bezoekers") |>
  filter(markt == params$markt) |>
  select(gemiddelde) |>
  pull()
