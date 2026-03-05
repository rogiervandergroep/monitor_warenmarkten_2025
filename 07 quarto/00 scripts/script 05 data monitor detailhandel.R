source("07 quarto/00 scripts/script 00 plot functies.R")


df_det <- read_rds("03 intermediate/tabellen_markt_basis.RDS")

df_det_markt <- df_det[["marktnaam"]] |>
  filter(
    monitor == 'monitor 2026',
    !is.na(v15_schoon),
    v15_schoon != 'onbekend',
    v15_schoon != 'anders'
  ) |>
  group_by(gebied_naam) |>
  slice_max(aandeel_gew, n = 5, with_ties = FALSE) |>
  mutate(aandeel_gew = aandeel_gew / 100)

df_det_markt |>
  filter(v15_schoon == 'bezoekt geen markt') |>
  write_rds("07 quarto/03 data/det_geenmrkt.rds")

df_det_markt |>
  filter(v15_schoon != 'bezoekt geen markt') |>
  group_by(gebied_naam) |>
  slice_max(aandeel_gew, n = 1, with_ties = FALSE) |>
  write_rds("07 quarto/03 data/det_meestgen.rds")


levels_stadsdeel_zwp |>
  map(\(x) {
    filter(
      df_det_markt,
      (gebied_naam == x | gebied_naam == 'Amsterdam')
    ) |>
      fun_totaal_een(
        xvar = aandeel_gew,
        yvar = fct_relevel(
          fct_reorder(v15_schoon, aandeel_gew),
          "bezoekt geen markt"
        ),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(
        ~ fct_relevel(gebied_naam, "Amsterdam", after = Inf),
        scales = 'free_y'
      )
  }) |>
  set_names(levels_stadsdeel_zwp) |>
  write_rds("07 quarto/02 figuren/fig_v_bez_markt_sd.rds")


#### redenen niet naar de markt

langezin <-
  "ik koop (bijna) al mijn boodschappen in de supermarkt of online, dus het is niet nodig om naar de markt te gaan"

kortezin <-
  "ik koop in de supermarkt of online"


df_redenniet <- read_rds("03 intermediate/tabellen_markt_geenreden.rds") |>
  filter(
    v14_cat != 'anders, namelijk',
    monitor == 'monitor 2026',
    !is.na(v14_cat)
  ) |>
  mutate(
    v14_cat = case_when(
      v14_cat == langezin ~ kortezin,
      v14_cat == 'te ver weg' ~ 'markt is te ver weg',
      v14_cat ==
        'de supermarkt is altijd open als ik boodschappen wil doen' ~ 'de supermarkt is altijd open',
      v14_cat ==
        'bij de supermarkt ligt alles wat ik nodig heb vlak bij elkaar' ~ 'bij de supermarkt ligt alles bij elkaar',

      TRUE ~ v14_cat
    )
  ) |>
  group_by(achtergrond_type) |>
  slice_max(aandeel, n = 6, with_ties = FALSE)


levels_stadsdeel_zwp |>
  map(\(x) {
    filter(
      df_redenniet,
      (achtergrond_type == x | achtergrond_type == 'totaal')
    ) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(
          fct_reorder(v14_cat, aandeel),
          "bezoekt geen markt"
        ),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(
        ~ fct_relevel(achtergrond_type, "totaal", after = Inf)
      )
  }) |>
  set_names(levels_stadsdeel_zwp) |>
  write_rds("07 quarto/02 figuren/fig_redengeen_sd.rds")


df_markt_prijs <- read_rds("03 intermediate/tabellen_markt_prijs.rds")


df_markt_prijs <- df_markt_prijs |>
  mutate(
    markt = case_when(
      markt == "Anton de Kompleinmarkt" ~ "Anton de Komplein",
      markt == "Buikslotermeerpleinmarkt" ~ "Buikslotermeerplein",
      markt == "Ganzenhoefmarkt" ~ "Ganzenhoef",
      markt == "Haarlemmerpleinmarkt" ~ "Haarlemmerplein",
      markt == "Kraaiennestmarkt" ~ "Kraaiennest",
      markt == "Lindengrachtmarkt" ~ "Lindengracht",
      markt == "Plein 40-45 markt" ~ "Plein '40-'45",
      markt == "Reigersbosmarkt" ~ "Reigersbos",
      markt == "Stadionplein" ~ "Stadionpleinmarkt",
      markt == "Ten Katestraatmarkt" ~ "Ten Katemarkt",
      markt == "Tussen Meer markt" ~ "Tussen Meer",
      markt == "Van Eesterenlaanmarkt" ~ "Biomarkt Zeeburg",
      TRUE ~ markt
    )
  )


write_rds(df_markt_prijs, "07 quarto/03 data/tabellen_markt_prijs.rds")


#
levels_markt |>
  map(\(x) {
    filter(
      df_markt_prijs,
      (markt == x | markt == 'totaal')
    ) |>
      fun_totaal_een(
        xvar = uitgaven,
        yvar = fct_rev(monitor),
        verm_factor = 1
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(
        labels = scales::label_dollar(
          prefix = "€",
          suffix = ",-"
        )
      ) +
      facet_wrap(
        ~ fct_relevel(markt, "totaal", after = Inf)
      )
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_markt_prijs.rds")
