# figuren v1 frequentie

source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")

tabel_v1 <- read_rds("03 intermediate/markten_v1_freq.rds")


# selectie totaal -
tabel_v1 |>
  filter(
    groep == 'bezoekers',
    markt == 'totaal',
    leefklas == 'totaal',
    locatie == 'totaal'
  ) |>
  mutate(v1 = factor(v1, levels = freq_levels_bez)) |>
  fun_totaal(
    xvar = perc,
    yvar = fct_rev(jaar),
    fill = fct_rev(v1),
    color_pal = os_blauw
  ) +
  facet_wrap(~type_markt2) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  )
ggsave("06 output figuren/fig_v1_totaal.svg", width = 12, height = 4)


tabel_v1 |>
  filter(
    leefklas != 'totaal',
    groep == 'bezoekers',
    markt == 'totaal',
    type_markt2 == 'totaal'
  ) |>
  mutate(
    v1 = factor(v1, levels = freq_levels_bez),
    leefklas = factor(leefklas, levels = leefklas_lev)
  ) |>
  fun_totaal(
    xvar = perc,
    yvar = fct_rev(jaar),
    fill = fct_rev(v1),
    color_pal = os_blauw
  ) +
  facet_wrap(~leefklas, nrow = 1) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  )
ggsave("06 output figuren/fig_v1_leefkl.svg", width = 12, height = 4)

# figuur naar locatie -
tabel_v1 |>
  filter(
    locatie != 'totaal',
    groep == 'bezoekers',
    markt == 'totaal',
    type_markt2 == 'totaal'
  ) |>
  mutate(
    v1 = factor(v1, levels = freq_levels_bez),
    locatie = factor(locatie, levels = loc_levels_lang)
  ) |>
  fun_totaal(
    xvar = perc,
    yvar = fct_rev(jaar),
    fill = fct_rev(v1),
    color_pal = os_blauw
  ) +
  facet_wrap(~locatie, nrow = 1) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  )
ggsave("06 output figuren/fig_v1_loc.svg", width = 12, height = 4)


# figuur naar meerdaagse markt
tabel_v1 |>
  filter(
    markt != 'Pekmarkt',
    markt != 'Zuidasmarkt',
    markt != 'Westerstraat',
    markt != 'Nieuwmarkt',
    markt != 'Hermitagemarkt',
    type_markt2 == 'eendaagse markt',
    locatie == 'totaal',
    groep == 'bezoekers',
    markt != 'totaal'
  ) |>
  mutate(
    v1 = factor(v1, levels = freq_levels_bez),
    locatie = factor(locatie, levels = loc_levels_lang)
  ) |>
  fun_totaal(
    xvar = perc,
    yvar = fct_rev(markt),
    fill = fct_rev(v1),
    color_pal = os_blauw
  ) +
  facet_wrap(~jaar, nrow = 1) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  )
ggsave("06 output figuren/fig_markt_freq_dg.svg", width = 12, height = 6)


# figuur naar meerdaagse markt
tabel_v1 |>
  filter(
    markt != 'Pekmarkt',
    markt != 'Zuidasmarkt',
    markt != 'Westerstraat',
    markt != 'Nieuwmarkt',
    markt != 'Hermitagemarkt',
    type_markt2 == 'markt op meerdere dagen',
    locatie == 'totaal',
    groep == 'bezoekers',
    markt != 'totaal'
  ) |>
  mutate(
    v1 = factor(v1, levels = freq_levels_bez),
    locatie = factor(locatie, levels = loc_levels_lang)
  ) |>
  fun_totaal(
    xvar = perc,
    yvar = fct_rev(markt),
    fill = fct_rev(v1),
    color_pal = os_blauw
  ) +
  facet_wrap(~jaar, nrow = 1) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  )
ggsave("06 output figuren/fig_markt_freq_meerdaags.svg", width = 12, height = 6)
