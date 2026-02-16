### vraag 4 ondernemers ---
### ontwikkeling bezoekersaantallen toe of afgenomen

source("07 quarto/00 scripts/script 00 plot functies.R")


tabel_v4_ond <- read_rds("03 intermediate/tabel_v4a_ond_voorachter.rds")

tabel_v4 <- bind_rows(
  tabel_v4_ond[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v4_ond[["markt"]]
)

# percentage voor in tekst
tabel_v4_ond_toename <- tabel_v4 |>
  filter(jaar == 'jaar 2025') |>
  filter(v4a %in% c('sterk toegenomen', 'toegenomen')) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_v4_toename.rds")

# percentage voor in tekst
tabel_v4_ond_afname <- tabel_v4 |>
  filter(jaar == 'jaar 2025') |>
  filter(v4a %in% c('sterk afgenomen', 'afgenomen')) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_v4_afname.rds")


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


### beter slechter dan andere markten

tabel_v7_beterslecht <- read_rds(
  "03 intermediate/tabel_v7_ond_beterslechter.rds"
)


tabel_v7 <- bind_rows(
  tabel_v7_beterslecht[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v7_beterslecht[["markt"]]
)

#aandeel (beter en veel beter dan andere markten)
tabel_v7_max <- tabel_v7 |>
  filter(jaar == 'jaar 2025') |>
  filter(v7 %in% c("veel beter", "beter")) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_v7_max.rds")


tabel_v7 |>
  my_stack_figure(vraag = v7, naam = "v7_beterslechter")


### v15 : markt op afstand

tabel_v15_marktopafstand <- read_rds(
  "03 intermediate/tabel_v15_ond_opafstand.rds"
)

tabel_15 <- bind_rows(
  tabel_v15_marktopafstand[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v15_marktopafstand[["markt"]]
)

#aandeel (beter en veel beter dan andere markten)
tabel_15_max <- tabel_15 |>
  filter(jaar == 'jaar 2025') |>
  filter(v15 %in% c("daar sta ik negatief tegenover")) |>
  write_rds("07 quarto/03 data/tab_ond_v15_afst.rds")


tabel_15 |>
  my_stack_figure_eenjaar(vraag = v15, naam = "v15_opafstand")

#
#
#  v16a ziet u zich nog over tien jaar?

tabel_v16_tienjaar <- read_rds(
  "03 intermediate/tabel_v16a_ond_tienjaar.rds"
)

tabel_16 <- bind_rows(
  tabel_v16_tienjaar[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v16_tienjaar[["markt"]]
)

tabel_16_max <- tabel_16 |>
  filter(jaar == 'jaar 2025') |>
  filter(v16a %in% c("ja, zeker", "ja, waarschijnlijk wel")) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_v16_tienjr.rds")


tabel_16 |>
  my_stack_figure_eenjaar(vraag = v16a, naam = "v16_tienjaar")

#  v16b toelichting?
