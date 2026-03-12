### vraag 4 ondernemers ---
### ontwikkeling bezoekersaantallen toe of afgenomen

source("07 quarto/00 scripts/script 00 plot functies.R")


tabel_v4_ond <- read_rds("03 intermediate/tabel_v4a_ond_voorachter.rds")

tabel_v4 <- bind_rows(
  tabel_v4_ond[["totaal"]] |>
    add_column(markt = 'alle markten'),
  tabel_v4_ond[["markt"]] |>
    my_markt_rename()
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


levels_markt |>
  map(\(x) {
    filter(tabel_v4, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v4a),
        color_pal = stoplicht6[c(1, 2, 3, 4, 5, 7)]
      ) +
      facet_wrap(~ fct_relevel(markt, 'alle markten', after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v4_ontw_ond.rds")


### vraag 5 ondernemers ---
### samenwerking ondernemers

tabel_v5_ond <- read_rds("03 intermediate/tabel_v5_ond_samenwerking.rds")

tabel_v5 <- bind_rows(
  tabel_v5_ond[["totaal"]] |>
    add_column(markt = 'alle markten'),
  tabel_v5_ond[["markt"]] |>
    my_markt_rename()
)

## ondernemers
levels_markt |>
  map(\(x) {
    filter(
      tabel_v5,
      samenwerking_met == 'ondernemers',
      markt %in% c(x, 'alle markten')
    ) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v5),
        color_pal = stoplicht6[c(1, 2, 3, 5, 7)]
      ) +
      facet_wrap(~ fct_relevel(markt, 'alle markten', after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v5_samenw_ondernemers.rds")

## winkeliers
levels_markt |>
  map(\(x) {
    filter(
      tabel_v5,
      samenwerking_met == 'winkeliers',
      markt %in% c(x, 'alle markten')
    ) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v5),
        color_pal = stoplicht6[c(1, 2, 3, 5, 7)]
      ) +
      facet_wrap(~ fct_relevel(markt, 'alle markten', after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v5_samenw_winkeliers.rds")

## horeca
levels_markt |>
  map(\(x) {
    filter(
      tabel_v5,
      samenwerking_met == 'horeca',
      markt %in% c(x, 'alle markten')
    ) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v5),
        color_pal = stoplicht6[c(1, 2, 3, 5, 7)]
      ) +
      facet_wrap(~ fct_relevel(markt, 'alle markten', after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v5_samenw_horeca.rds")

## stadsdeel
levels_markt |>
  map(\(x) {
    filter(
      tabel_v5,
      samenwerking_met == 'stadsdeel, gemeente',
      markt %in% c(x, 'alle markten')
    ) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v5),
        color_pal = stoplicht6[c(1, 2, 3, 5, 7)]
      ) +
      facet_wrap(~ fct_relevel(markt, 'alle markten', after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v5_samenw_stadsdeel.rds")


### beter slechter dan andere markten

tabel_v7_beterslecht <- read_rds(
  "03 intermediate/tabel_v7_ond_beterslechter.rds"
)


tabel_v7 <- bind_rows(
  tabel_v7_beterslecht[["totaal"]] |>
    add_column(markt = 'alle markten'),
  tabel_v7_beterslecht[["markt"]] |>
    my_markt_rename()
) |>
  filter(v7 != 'niet ingevuld')

#aandeel (beter en veel beter dan andere markten)
tabel_v7_max <- tabel_v7 |>
  filter(jaar == 'jaar 2025') |>
  filter(v7 %in% c("veel beter", "beter")) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_v7_max.rds")


levels_markt |>
  map(\(x) {
    filter(tabel_v7, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fill = fct_rev(v7),
        color_pal = stoplicht6[c(1, 2, 3, 4, 5, 7)]
      ) +
      facet_wrap(~ fct_relevel(markt, 'alle markten', after = Inf)) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v7_beterslechter.rds")


### v15 : markt op afstand

tabel_v15_marktopafstand <- read_rds(
  "03 intermediate/tabel_v15_ond_opafstand.rds"
)

tabel_15 <- bind_rows(
  tabel_v15_marktopafstand[["totaal"]] |>
    add_column(markt = 'alle markten'),
  tabel_v15_marktopafstand[["markt"]] |>
    my_markt_rename()
) |>
  mutate(
    v15 = case_when(
      v15 ==
        'daar sta ik neutraal tegenover: niet positief maar ook niet negatief' ~ 'daar sta ik neutraal tegenover',
      TRUE ~ v15
    )
  )

#aandeel (beter en veel beter dan andere markten)
tabel_15_max <- tabel_15 |>
  filter(jaar == 'jaar 2025') |>
  filter(v15 %in% c("daar sta ik negatief tegenover")) |>
  write_rds("07 quarto/03 data/tab_ond_v15_afst.rds")


levels_markt |>
  map(\(x) {
    filter(tabel_15, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, 'alle markten'),
        fill = fct_rev(v15),
        color_pal = stoplicht6[c(7, 4, 2)]
      ) +
      guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v15_opafstand.rds")

#
#
#  v16a ziet u zich nog over tien jaar?

tabel_v16_tienjaar <- read_rds(
  "03 intermediate/tabel_v16a_ond_tienjaar.rds"
)

tabel_16 <- bind_rows(
  tabel_v16_tienjaar[["totaal"]] |>
    add_column(markt = 'alle markten'),
  tabel_v16_tienjaar[["markt"]] |>
    my_markt_rename()
)

tabel_16_max <- tabel_16 |>
  filter(jaar == 'jaar 2025') |>
  filter(v16a %in% c("ja, zeker", "ja, waarschijnlijk wel")) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_v16_tienjr.rds")


levels_markt |>
  map(\(x) {
    filter(tabel_16, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, 'alle markten'),
        fill = fct_rev(v16a),
        color_pal = stoplicht6[c(2, 3, 5, 7)]
      ) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v16_tienjaar.rds")

#  v16b toelichting?
