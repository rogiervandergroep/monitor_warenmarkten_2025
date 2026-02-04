### frequentie monitor detailhandel

# source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")

os_blauw <- c(
  "#e6e6e6",
  "#dcddee",
  "#b8bcdd",
  "#959dcc",
  "#707ebb",
  "#4861aa",
  "#004699"
)

my_markt_selection <- function(x, markt_selectie) {
  bind_rows(
    # markt
    x |>
      filter(groep == 'bezoekers') |>
      filter(markt == markt_selectie) |>
      filter(leefklas == 'totaal') |>
      filter(locatie == 'totaal'),

    # totaal
    x |>
      filter(
        groep == 'bezoekers',
        markt == 'totaal',
        type_markt2 == 'totaal',
        leefklas == 'totaal',
        locatie == 'totaal'
      ),

    # meerdaags of eendaags
    x |>
      filter(
        groep == 'bezoekers',
        markt == 'totaal',
        leefklas == 'totaal',
        locatie == 'totaal'
      ) |>
      filter(
        if (markt_selectie %in% levels_markt_eendaags) {
          type_markt2 == 'eendaagse markt'
        } else {
          type_markt2 == 'markt op meerdere dagen'
        }
      ) |>
      dplyr::select(-c("markt")) |>
      rename(markt = type_markt2)
  )
}


## stack figuur markt links totaal rechts

my_stack_figure <- function(tabel, vraag, naam) {
  levels_markt |>
    map(\(x) {
      filter(tabel, markt %in% c(x, "totaal")) |>
        fun_totaal(
          xvar = aandeel,
          yvar = fct_rev(jaar),
          fillvar = fct_rev({{ vraag }}),
          color_pal = os_blauw
        ) +

        facet_wrap(~ fct_relevel(markt, "totaal", after = Inf)) +
        guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
    }) |>
    set_names(levels_markt) |>
    write_rds(glue::glue("07 quarto/02 figuren/fig_{ naam }.rds"))
}


#### vraag 1: frequentie ---

tabel_v1 <- read_rds("03 intermediate/markten_v1_freq.rds")


levels_markt |>
  map(\(x) {
    filter(tabel_v3, groep == 'bezoekers', (markt == x | markt == 'totaal')) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(fct_reorder(name_tot, aandeel), "anders"),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v3_redenen_bez.rds")


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
        color_pal = os_blauw
      ) +
      facet_wrap(
        ~ fct_relevel(
          markt,
          "eendaagse markt",
          "markt op meerdere dagen",
          "totaal",
          after = Inf
        ),
        nrow = 1
      ) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v1_freq.rds")


#### vraag 3 : redenen bezoek ---

tabel_v3 <- read_rds("03 intermediate/markten_v3_redenbezoek_alles.rds")
# toevegen : reden ondernemers
levels_markt |>
  map(\(x) {
    filter(tabel_v3, groep == 'bezoekers', (markt == x | markt == 'totaal')) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(fct_reorder(name_tot, aandeel), "anders"),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v3_redenen_bez.rds")

levels_markt |>
  map(\(x) {
    filter(
      tabel_v3,
      groep == 'ondernemers',
      (markt == x | markt == 'totaal')
    ) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(fct_reorder(name_tot, aandeel), "anders"),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v3_redenen_ond.rds")

# anders namelijk
read_rds("03 intermediate/markten_v3_redenbezoek_anders.rds") |>
  group_by(markt) |>
  summarise(v3 = paste(unique(value), collapse = "; ")) |>
  write_rds("07 quarto/03 data/tab_v3_reden_anders.rds")

# toelichting bij gezellig
read_rds("03 intermediate/markten_v3_redenbezoek_gezellig.rds") |>
  group_by(markt) |>
  summarise(v3 = paste(unique(value), collapse = "; ")) |>
  write_rds("07 quarto/03 data/tab_v3_reden_gezellig.rds")


# vraag 4: wat kopen bezoekers op de markt

tab_v4_producten <- read_rds("03 intermediate/tab_markten_v4_prod.rds") |>
  filter(labels != 'weet niet, geen antwoord')


tab_v4_max <- tab_v4_producten |>
  filter(labels != 'anders') |>
  group_by(markt) |>
  slice_max(aandeel, n = 1, with_ties = F) |>
  write_rds("07 quarto/03 data/tab_v4_max.rds")


levels_markt |>
  map(\(x) {
    filter(tab_v4_producten, markt %in% c(x, 'totaal')) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(fct_reorder(labels, aandeel), "anders")
      ) +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf)) +
      scale_x_continuous(labels = scales::percent) +
      guides(color = 'none', fill = 'none')
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v4_producten.rds")

# andere zaken die op de markt gekocht zijn
tab_v4_anders <- read_rds("03 intermediate/tab_markten_v4_prod_anders.rds") |>
  group_by(markt) |>
  summarise(v4 = paste(unique(v4_other15), collapse = "; ")) |>
  write_rds("07 quarto/03 data/tab_v4_andereprod.rds")


### vraag 4 ondernemers ---
### ontwikkeling bezoekersaantallen toe of afgenomen

tabel_v4_ond <- read_rds("03 intermediate/tabel_v4a_ond_voorachter.rds")

tabel_v4 <- bind_rows(
  tabel_v4_ond[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v4_ond[["markt"]]
)

tabel_v4 |>
  my_stack_figure(vraag = v4a, naam = "v4_ontw_ond")


### vraag 5 ondernemers ---
### osamenwerking ondernemers

tabel_v5_ond <- read_rds("03 intermediate/tabel_v5_ond_samenwerking.rds")

tabel_v5 <- bind_rows(
  tabel_v5_ond[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v5_ond[["markt"]]
)

levels_markt |>
  map(\(x) {
    filter(tabel_v5, markt == x) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v5),
        color_pal = os_blauw
      ) +
      facet_wrap(~samenwerking_met) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T)) +
      labs(title = x)
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v5_samenw_ond_markt.rds")


test <- tabel_v5_ond[["totaal"]] |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fill = fct_rev(v5),
    color_pal = os_blauw
  ) +
  facet_wrap(~samenwerking_met) +
  guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T)) +
  labs(title = "totaal")

write_rds(test, "07 quarto/02 figuren/fig_v5_samenw_ond_totaal.rds")


#### vraag 5: voornamelijk markt of winkels

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
  )


levels_markt |>
  map(\(x) {
    filter(tabel_v5, achtergrond_type %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(v5),
        color_pal = os_blauw[c(1, 3, 5, 6, 7)]
      ) +
      facet_wrap(~ fct_relevel(achtergrond_type, "totaal", after = Inf)) +
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
    filter(tabel_v6, (achtergrond_type == x | achtergrond_type == "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(achtergrond_type),
        fillvar = fct_rev(v6),
        color_pal = os_blauw
      ) +
      labs(title = "vervoermiddel") +
      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v6_vervoermiddel.rds")


## bezoeker Gaat ook naar andere markt

tabel_v14_anderemarkt <- read_rds(
  "03 intermediate/tabel_v14_anderemarkt.rds"
) |>
  filter(
    v14 != 'ik kan de markt niet vinden in deze lijst',
    v14 != "geen andere markten"
  ) |>
  group_by(markt) |>
  slice_max(aandeel, n = 3)


read_rds("03 intermediate/tabel_v14_anderemarkt.rds") |>
  filter(v14 == "geen andere markten") |>
  write_rds("07 quarto/03 data/tab_geenanderemarkt.rds")

read_rds("03 intermediate/tabel_v14_anderemarkt.rds") |>
  filter(
    v14 != 'ik kan de markt niet vinden in deze lijst',
    v14 != "geen andere markten"
  ) |>
  group_by(markt) |>
  slice_max(aandeel, n = 1) |>
  write_rds("07 quarto/03 data/tab_meestgenoemdeanderemarkt.rds")


levels_markt |>
  map(\(x) {
    filter(tabel_v14_anderemarkt, markt == x) |>
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
