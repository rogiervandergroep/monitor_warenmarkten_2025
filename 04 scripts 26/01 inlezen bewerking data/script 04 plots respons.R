#### figure respons ---

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")
respons <- read_rds("03 intermediate/markten_respons.rds")


# selectie leeftijd bezoekers per martk en totaal
respons[["leeftijd"]] |>
  filter(
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  mutate(
    aandeel = aandeel * 100
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_relevel(fct_rev(markt), "totaal"),
    fill = fct_rev(leefklas),
    color_pal = os_blauw[c(1, 3, 4, 6, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  ) +
  theme_os(orientation = 'horizontal')

# totaal amsterdam alle jaren alleen bezoekers
respons[["leeftijd"]] |>
  filter(
    markt == 'totaal',
    groep %in% c('bezoekers')
  ) |>
  mutate(
    aandeel = aandeel
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fill = fct_rev(leefklas),
    color_pal = os_blauw[c(1, 3, 5, 7, 10)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(ncol = 2, reverse = T)
  ) +
  theme_os(orientation = 'horizontal')

ggsave("06 output figuren/fig_respons_lft_ams.svg", width = 6, height = 4)

# "#ec0000" "#ff9100" "#d48fb9" "#fdb0cb" "#ffe600" "#bed200" "#6cbd74" "#009dec" "#004699" "#e6e6e6"
# "#004699" "#3858a4" "#566bb0" "#707ebb" "#8992c6" "#a1a7d2" "#b8bcdd" "#d0d2e8" "#e7e8f4"
# "#e6e6e6" "#e7e8f4" "#d0d2e8" "#b8bcdd" "#a1a7d2" "#8992c6" "#707ebb" "#566bb0" "#3858a4" "#004699"

# selectie locatie per markt en totaal -
respons[["locatie"]] |>
  filter(
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  mutate(
    aandeel = aandeel * 100,
    locatie = factor(locatie, levels = levels_loc_lang)
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_relevel(fct_rev(markt), "totaal"),
    fill = fct_rev(locatie),
    color_pal = os_blauw[c(1, 3, 4, 5, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 1, reverse = T)
  ) +
  theme_os(orientation = 'horizontal')

# selectie amsterdam alle jaren
respons[["locatie"]] |>
  filter(
    markt == 'totaal',
    groep == 'bezoekers'
  ) |>
  mutate(
    aandeel = aandeel
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fill = fct_rev(fct_relevel(locatie, levels_loc_lang)),
    color_pal = discreet[c(10, 5, 7, 3, 1)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(ncol = 2, reverse = T)
  ) +
  theme_os(orientation = 'horizontal', legend_position = 'bottom')

ggsave("06 output figuren/fig_respons_loc.svg", width = 6, height = 4)


# selectie totaal -
respons[["stadsdeel"]] |>
  filter(
    gebied_stadsdeel_code != 'B',
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  mutate(
    aandeel = aandeel,
    gebied_stadsdeel_naam = factor(
      gebied_stadsdeel_naam,
      levels = levels_stadsdeel
    )
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(gebied_stadsdeel_naam),
    fill = os_blauw[c(7)],
    color_pal = os_blauw[c(7)]
  ) +
  guides(
    color = 'none',
    fill = 'none'
  ) +
  theme_os(orientation = 'horizontal', legend_position = 'right')


#### ondernemers ---

a <- respons[["ond_lengte"]] |>
  filter(
    markt == 'totaal'
  ) |>
  mutate(
    aandeel = aandeel * 100
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fill = fct_rev(v1),
    color_pal = os_blauw[c(1, 2, 4, 5, 6, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(ncol = 2, reverse = T)
  ) +
  theme_os(orientation = 'horizontal', legend_position = 'bottom')

b <- respons[["ond_plek"]] |>
  filter(
    markt == 'totaal'
  ) |>
  mutate(
    aandeel = aandeel * 100
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fill = fct_rev(v2),
    color_pal = os_blauw[c(1, 4, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(ncol = 1, reverse = T)
  ) +
  theme_os(orientation = 'horizontal', legend_position = 'bottom')


c <- respons[["ond_verkoop"]] |>
  filter(
    markt == 'totaal'
  ) |>
  mutate(
    aandeel = aandeel * 100
  ) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fill = fct_rev(v3),
    color_pal = os_blauw[c(1, 4, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(ncol = 1, reverse = T)
  ) +
  theme_os(orientation = 'horizontal', legend_position = 'bottom')


library(patchwork)

a + b + c


# ggsave("06 output figuren/fig_respons_ond.svg", width = 12, height = 5)

respons[["engels"]] |>
  filter(
    gebied_stadsdeel_naam != 'Westpoort',
    gebied_stadsdeel_naam != 'woonplaats onbekend'
  ) |>
  mutate(
    gebied_stadsdeel_naam = factor(
      gebied_stadsdeel_naam,
      levels = c(
        levels_stadsdeel_zwp,
        "overig Nederland",
        "buitenland"
      )
    )
  ) |>

  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(gebied_stadsdeel_naam),
    fill = a,
    color_pal = discreet[c(1, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 1, reverse = T)
  ) +
  facet_wrap(~jaar) +
  theme_os(orientation = 'horizontal', legend_position = 'bottom')
