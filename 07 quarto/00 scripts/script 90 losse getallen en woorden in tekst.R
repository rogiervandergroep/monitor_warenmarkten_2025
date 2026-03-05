grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"

stoplicht6 <- c(
  "#e6e6e6",
  "#ec0000",
  "#f28b21",
  "#f8d376",
  "#e2e693",
  "#a1cd73",
  "#53b361"
)
os_blauw <- c(
  "#e6e6e6",
  "#e7e8f4",
  "#d0d2e8",
  "#b8bcdd",
  "#a1a7d2",
  "#8992c6",
  "#707ebb",
  "#566bb0",
  "#3858a4",
  "#004699"
)

discreet <- c(
  "#ec0000",
  "#ff9100",
  "#d48fb9",
  "#fdb0cb",
  "#ffe600",
  "#bed200",
  "#6cbd74",
  "#009dec",
  "#004699",
  "#e6e6e6"
)


my_markt_rename <- function(x) {
  x |>
    mutate(
      markt = case_when(
        markt == "totaal" ~ "alle markten",
        markt == "Plein 40-45" ~ "Plein '40-'45",
        markt == "Tussenmeer" ~ "Tussen Meer",
        TRUE ~ markt
      )
    )
}


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

### respons

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


bez_lft <- read_rds("03 data/tab_bez_lft35.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

ond_lft <- read_rds("03 data/tab_ond_lft56.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

herk <- read_rds("03 data/tab_bez_herk.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


### getallen marktbureau ---

mb_getallen <- read_rds("03 data/tab_mb_getallen.rds")

mb_verg <- mb_getallen$aandeel_vergun |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

mb_17jr <- mb_getallen$aandeel_17 |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

mb_60jr <- mb_getallen$aandeel_60 |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


# meest genoemde vervoermiddel
verv <- read_rds("03 data/tab_verv.rds") |>
  filter(achtergrond_type == params$markt) |>
  select(v6) |>
  pull()

# aandeel vergunninghouder

# lengte markt

lengte_ond <- read_rds("03 data/tab_ond_lengte10.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

vast <- read_rds("03 data/tab_v0_vast.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


### v3 reden bezoek volgens bezoeker en ondernemers

# anders
v3_bez_max <- read_rds("03 data/tab_v3_reden_max.rds") |>
  filter(
    groep == 'bezoekers',
    markt == params$markt
  ) |>
  select(name_tot) |>
  pull()

v3_ond_max <- read_rds("03 data/tab_v3_reden_max.rds") |>
  filter(
    groep == 'ondernemers',
    markt == params$markt
  ) |>
  select(name_tot) |>
  pull()


# open antwoorden vraag 3 reden bezoek

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

### redenen bezoeker ontevrden
tab_v9_max <- read_rds("03 data/tab_v9_reden_ont_max.rds") |>
  filter(markt == params$markt) |>
  select(labels) |>
  pull()

# aandeel toegenomen en afgenomen
tab_v3_afname <- read_rds("03 data/tab_ond_v4_afname.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

tab_v3_toename <- read_rds("03 data/tab_ond_v4_toename.rds") |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


# open antwoorden bij vraag of er sprake if van toename of afname bezoekers
tab_v4b_toename <- read_rds("03 data/tab_reden_on_4b.rds") |>
  my_markt_rename() |>
  filter(
    v4a == 'toegenomen',
    markt == params$markt
  ) |>
  select(v4b_reden) |>
  pull()

tab_v4b_afname <- read_rds("03 data/tab_reden_on_4b.rds") |>
  my_markt_rename() |>
  filter(
    v4a == 'afgenomen',
    markt == params$markt
  ) |>
  select(v4b_reden) |>
  pull()

# v6 toelichting bij vraag v5: samenwerking markt
tab_v6_toelichting <- read_rds("03 data/tab_v6_ond_toel.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(v6_open) |>
  pull()

# v7 aaandeel beter en veel beter dan andere markten
tab_v7_max <- read_rds("03 data/tab_ond_v7_max.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

### anders : wat mist er volgens ondernemers

tab_v10_mist1 <- read_rds("03 data/tab_v10_ond_mist_anders.rds") |>
  my_markt_rename() |>
  filter(
    markt == params$markt
  ) |>
  select(v10_other12) |>
  pull()

tab_v10_mist2 <- read_rds("03 data/tab_v10_ond_mist_anders.rds") |>
  filter(
    markt == params$markt
  ) |>
  select(v10_other13) |>
  pull()


# sterk en zwakke punten volgens ondernemers -

tab_v11_sterk <- read_rds("03 data/tab_v11_sterk_open.rds") |>
  my_markt_rename() |>
  filter(
    markt == params$markt
  ) |>
  select(v11_open) |>
  pull()

tab_v12_zwak <- read_rds("03 data/tab_v12_zwak_open.rds") |>
  my_markt_rename() |>
  filter(
    markt == params$markt
  ) |>
  select(v12_open) |>
  pull()


#33 aandeel negatief afstand
tab_v15_neg_afst <- read_rds("03 data/tab_ond_v15_afst.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()

tab_v16_tienjr <- read_rds("03 data/tab_ond_v16_tienjr.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


### v16 toelichting of ondernemer er na tien jaar nog staat ---
tab_v16_open_ja <- read_rds("03 data/tab_v16_open.rds") |>
  my_markt_rename() |>
  filter(
    v16a == 'ja',
    markt == params$markt
  ) |>
  select(v16_open) |>
  pull()


### v16 toelichting of ondernemer er na tien jaar nog staat ---
tab_v16_open_nee <- read_rds("03 data/tab_v16_open.rds") |>
  my_markt_rename() |>
  filter(
    v16a == 'nee',
    markt == params$markt
  ) |>
  select(v16_open) |>
  pull()


### opmerkingen bezoekers en ondernemers
tab_ond_opm <- read_rds("03 data/tab_ond_opmerkingen.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(opmerkingen) |>
  pull()

### opmerkingen bezoekers en ondernemers
tab_bez_opm <- read_rds("03 data/tab_bez_opmerkingen.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(opmerkingen) |>
  pull()


# meest genoemde vervoermiddel
verv <- read_rds("03 data/tab_verv.rds") |>
  mutate(
    achtergrond_type = case_when(
      achtergrond_type == "totaal" ~ "alle markten",
      achtergrond_type == "Plein 40-45" ~ "Plein '40-'45",
      achtergrond_type == "Tussenmeer" ~ "Tussen Meer",
      TRUE ~ achtergrond_type
    )
  ) |>
  filter(achtergrond_type == params$markt) |>
  select(v6) |>
  pull()


# meest genoemde vervoermiddel
geenmarkt <- read_rds("03 data/tab_geenanderemarkt.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


meestanderemarkt <- read_rds("03 data/tab_meestgenoemdeanderemarkt.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(v14) |>
  pull()


v8_goed <- read_rds("03 data/v8_goed.rds") |>
  my_markt_rename() |>
  filter(markt == params$markt) |>
  select(aandeel) |>
  pull()


### rapportcijfers ---

rap_cijfers <- read_rds("03 data/tab_rapportcijfers.rds") |>
  my_markt_rename()

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
