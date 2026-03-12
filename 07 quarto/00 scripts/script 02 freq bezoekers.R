### frequentie monitor detailhandel

source("07 quarto/00 scripts/script 00 plot functies.R")

#### vraag 1: frequentie ---

tabel_v1 <- read_rds("03 intermediate/markten_v1_freq.rds") |>
  my_markt_rename()

tabel_v1 |>
  filter(
    groep == 'bezoekers',
    jaar == 'jaar 2025',
    leefklas == 'totaal',
    locatie == 'totaal'
  ) |>
  filter(v1 %in% c("1 keer per week", "een aantal keer per week")) |>
  group_by(type_markt2, markt) |>
  summarise(freq = sum(perc)) |>
  write_rds("07 quarto/03 data/tab_freq.rds")


levels_markt |>
  map(\(x) {
    my_markt_selection(tabel_v1, x) |>
      mutate(
        v1 = factor(v1, levels = levels_freq_bez),
        locatie = factor(locatie, levels = levels_loc_lang)
      ) |>
      fun_totaal(
        xvar = perc / 100,
        yvar = fct_rev(jaar),
        fill = fct_rev(v1),
        color_pal = stoplicht6[c(1, 6, 5, 4, 3, 2)]
      ) +
      facet_wrap(
        ~ fct_relevel(
          markt,
          "eendaagse markt",
          "markt op meerdere dagen",
          "alle markten",
          after = Inf
        ),
        nrow = 1
      ) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v1_freq.rds")

freq_figures <- read_rds("07 quarto/02 figuren/fig_v1_freq.rds")

freq_figures[["Plein '40-'45"]]
ggsave("07 quarto/02 figuren/fig_frq_plein4045.svg", width = 12, height = 4)

#################################
#### vraag 3 : redenen bezoek ---
#################################

tabel_v3 <- read_rds("03 intermediate/markten_v3_redenbezoek_alles.rds") |>
  my_markt_rename() |>
  filter(name_tot != 'weet niet, geen antwoord')

### meestgenoemede reden bezoek

tabel_v3_max <- tabel_v3 |>
  group_by(markt, groep) |>
  filter(
    name_tot != 'anders',
    name_tot != 'boodschappen doen'
  ) |>
  slice_max(aandeel, n = 1, with_ties = F) |>
  write_rds("07 quarto/03 data/tab_v3_reden_max.rds")


# redenen bezoekers
volgorde <- tabel_v3 |>
  ungroup() |>
  filter(
    markt == 'alle markten',
    groep == 'bezoekers'
  ) |>
  mutate(
    name_tot = fct_relevel(
      fct_reorder(name_tot, aandeel),
      "anders"
    )
  ) |>
  select(name_tot) |>
  pull()

figuur <- levels_markt |>
  map(\(x) {
    tabel_v3 |>
      mutate(name_tot = factor(name_tot, levels = volgorde)) |>
      filter(
        groep == 'bezoekers',
        (markt == x | markt == 'alle markten')
      ) |>
      fun_totaal_een(
        grenswaarde = 0.01,
        xvar = aandeel,
        yvar = fct_relevel(name_tot, levels(volgorde))
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf))
  }) |>
  set_names(levels_markt)

figuur[["volgorde"]] <- volgorde
write_rds(figuur, "07 quarto/02 figuren/fig_v3_redenen_bez.rds")

# redenen ondernemers
volgorde <- tabel_v3 |>
  ungroup() |>
  filter(
    markt == 'alle markten',
    groep == 'ondernemers'
  ) |>
  mutate(
    name_tot = fct_relevel(
      fct_reorder(name_tot, aandeel),
      "anders"
    )
  ) |>
  select(name_tot) |>
  pull()


# redenen ondernmers
figuur <- levels_markt |>
  map(\(x) {
    tabel_v3 |>
      mutate(name_tot = factor(name_tot, levels = volgorde)) |>
      filter(
        groep == 'ondernemers',
        (markt == x | markt == 'alle markten')
      ) |>
      fun_totaal_een(
        grenswaarde = 0.01,
        xvar = aandeel,
        yvar = fct_relevel(name_tot, levels(volgorde)),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf))
  }) |>
  set_names(levels_markt)

figuur[["volgorde"]] <- volgorde

write_rds(figuur, "07 quarto/02 figuren/fig_v3_redenen_ond.rds")

# anders namelijk
read_rds("03 intermediate/markten_v3_redenbezoek_anders.rds") |>
  my_markt_rename() |>
  group_by(markt) |>
  summarise(v3 = paste(unique(value), collapse = "; ")) |>
  write_rds("07 quarto/03 data/tab_v3_reden_anders.rds")

# toelichting bij gezellig
read_rds("03 intermediate/markten_v3_redenbezoek_gezellig.rds") |>
  my_markt_rename() |>
  group_by(markt) |>
  summarise(v3 = paste(unique(value), collapse = "; ")) |>
  write_rds("07 quarto/03 data/tab_v3_reden_gezellig.rds")

###################################################
#### vraag 4: wat kopen bezoekers op de markt -----
###################################################

tab_v4_producten <- read_rds("03 intermediate/tab_markten_v4_prod.rds") |>
  my_markt_rename() |>
  filter(labels != 'weet niet, geen antwoord') |>
  my_markt_rename()


tab_v4_max <- tab_v4_producten |>
  filter(labels != 'anders') |>
  group_by(markt) |>
  slice_max(aandeel, n = 1, with_ties = F) |>
  write_rds("07 quarto/03 data/tab_v4_max.rds")


volgorde <- tab_v4_producten |>
  ungroup() |>
  filter(
    markt == 'alle markten',
  ) |>
  mutate(
    labels = fct_relevel(
      fct_reorder(labels, aandeel),
      "anders"
    )
  ) |>
  select(labels) |>
  pull()


figuur <- levels_markt |>
  map(\(x) {
    tab_v4_producten |>
      mutate(labels = factor(labels, levels = volgorde)) |>
      filter(markt %in% c(x, 'alle markten')) |>
      fun_totaal_een(
        grenswaarde = 0.034,
        xvar = aandeel,
        yvar = fct_relevel(labels, levels(volgorde)),
      ) +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf)) +
      scale_x_continuous(labels = scales::percent) +
      guides(color = 'none', fill = 'none')
  }) |>
  set_names(levels_markt)

figuur[["volgorde"]] <- volgorde

write_rds(figuur, "07 quarto/02 figuren/fig_v4_producten.rds")

# andere zaken die op de markt gekocht zijn
tab_v4_anders <- read_rds("03 intermediate/tab_markten_v4_prod_anders.rds") |>
  my_markt_rename() |>
  group_by(markt) |>
  summarise(v4 = paste(unique(v4_other15), collapse = "; ")) |>
  write_rds("07 quarto/03 data/tab_v4_andereprod.rds")

################################################
#### vraag 5: voornamelijk markt of winkels ----
################################################

tabel_v5 <- read_rds("03 intermediate/tabel_v5_voornaamstedoel.rds") |>

  filter(achtergrond_var != 'gebied_stadsdeel_naam') |>
  mutate(v5 = str_remove_all(v5, " namelijk")) |>
  mutate(
    v5 = factor(
      v5,
      levels = c(
        "voornamelijk voor de markt",
        "voornamelijk voor de winkels in dit winkelgebied",
        "voornamelijk voor de supermarkten",
        "anders",
        "weet niet, geen antwoord"
      )
    )
  ) |>
  mutate(
    achtergrond_type = case_when(
      achtergrond_type == "totaal" ~ "alle markten",
      achtergrond_type == "Tussenmeer" ~ "Tussen Meer",
      achtergrond_type == "Plein 40-45" ~ "Plein '40-'45",
      TRUE ~ achtergrond_type
    )
  )


levels_markt |>
  map(\(x) {
    filter(tabel_v5, achtergrond_type %in% c(x, "alle markten")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(v5),
        color_pal = discreet[c(10, 4, 9, 8, 1)]
      ) +
      facet_wrap(~ fct_relevel(achtergrond_type, "alle markten", after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 3, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v5_markt_winkels.rds")


#####  ------

levels_v6 <- c(
  "te voet",
  "met de fiets",
  "met de brommer",
  "met de auto",
  "met het openbaar vervoer",
  "met de scootmobiel e.d.",
  "anders"
)


tabel_v6 <- read_rds("03 intermediate/tabel_v6_vervoermiddel.rds") |>
  mutate(
    achtergrond_type = case_when(
      achtergrond_type == "totaal" ~ "alle markten",
      achtergrond_type == "Tussenmeer" ~ "Tussen Meer",
      achtergrond_type == "Plein 40-45" ~ "Plein '40-'45",
      TRUE ~ achtergrond_type
    )
  ) |>
  filter(achtergrond_var %in% c('markt', 'totaal')) |>
  mutate(
    v6 = case_when(
      v6 == 'te voet' ~ 'te voet',
      v6 == 'fiets (gewoon of elektrisch)' ~ 'met de fiets',
      v6 == 'brommer, bromfiets, scooter' ~ 'met de brommer',
      v6 == 'auto of motor' ~ 'met de auto',
      v6 == 'openbaar vervoer' ~ 'met het openbaar vervoer',
      v6 == 'scootmobiel, rolstoel, canta, Birò' ~ 'met de scootmobiel e.d.',
      v6 == 'anders' ~ 'anders'
    )
  ) |>
  mutate(v6 = factor(v6, levels = levels_v6))


tabel_v6 |>
  group_by(achtergrond_type) |>
  slice_max(aandeel, n = 1) |>
  write_rds("07 quarto/03 data/tab_verv.rds")


levels_markt |>
  map(\(x) {
    filter(
      tabel_v6,
      (achtergrond_type == x | achtergrond_type == "alle markten")
    ) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(achtergrond_type, "alle markten"),
        fillvar = fct_rev(v6),
        color_pal = discreet[c(10, 9, 8, 6, 4, 3, 1)]
      ) +

      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v6_vervoermiddel.rds")


## bezoeker Gaat ook naar andere markt

tabel_v14_anderemarkt <- read_rds(
  "03 intermediate/tabel_v14_anderemarkt.rds"
) |>
  my_markt_rename()

tabel_14_top3 <- tabel_v14_anderemarkt |>
  filter(
    v14 != 'ik kan de markt niet vinden in deze lijst',
    v14 != "geen andere markten"
  ) |>
  group_by(markt) |>
  slice_max(aandeel, n = 3)

levels_markt |>
  map(\(x) {
    filter(tabel_14_top3, markt == x) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(
          fct_reorder(v14, aandeel),
          "geen andere markten",
          "ik kan de markt niet vinden in deze lijst"
        ),
        afr = 0
      ) +
      scale_x_continuous(labels = scales::percent) +
      guides(color = 'none', fill = 'none')
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v14_anderemarkt.rds")


tabel_v14_anderemarkt |>
  filter(v14 == "geen andere markten") |>
  write_rds("07 quarto/03 data/tab_geenanderemarkt.rds")

tabel_v14_anderemarkt |>
  filter(
    v14 != 'ik kan de markt niet vinden in deze lijst',
    v14 != "geen andere markten"
  ) |>
  group_by(markt) |>
  slice_max(aandeel, n = 1) |>
  write_rds("07 quarto/03 data/tab_meestgenoemdeanderemarkt.rds")
