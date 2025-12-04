### berekening respons per markt ---

library(tidyverse)
library(openxlsx)

## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")

respons <- list()


respons[["totaal"]] <- my_bind_rows(groupvars = NULL) |>
  group_by(jaar, type_markt2, markt, groep) |>
  summarise(aantal = n()) |>
  pivot_wider(names_from = groep, values_from = aantal)

respons[["stadsdeel"]] <- my_bind_rows(
  groupvars = c("gebied_stadsdeel_code", "gebied_stadsdeel_naam")
) |>
  group_by(jaar, groep, gebied_stadsdeel_code, gebied_stadsdeel_naam) |>
  summarise(aantal = n()) |>
  group_by(jaar, groep) |>
  mutate(aandeel = aantal / sum(aantal))

respons[["leeftijd"]] <- bind_rows(
  my_bind_rows(groupvars = NULL) |>
    group_by(jaar, type_markt2, markt, groep, leefklas) |>
    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt, groep) |>
    mutate(aandeel = aantal / sum(aantal)),

  my_bind_rows(groupvars = NULL) |>
    group_by(jaar, groep, leefklas) |>
    summarise(aantal = n()) |>
    group_by(jaar, groep) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
)

respons[["locatie"]] <- bind_rows(
  my_bind_rows(groupvars = NULL) |>
    group_by(jaar, type_markt2, markt, groep, locatie) |>
    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt, groep) |>
    mutate(aandeel = aantal / sum(aantal)),

  my_bind_rows(groupvars = NULL) |>
    group_by(jaar, groep, locatie) |>
    summarise(aantal = n()) |>
    group_by(jaar, groep) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
)


write.xlsx(respons, "05 output tabellen/tabel_respons_overzicht.xlsx")


#### figure respons ---

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")

# selectie totaal -
respons[["leeftijd"]] |>
  filter(
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  mutate(
    aandeel = aandeel * 100,
    leefklas = replace_na(leefklas, "onbekend"),
    leefklas = factor(leefklas, levels = levels_leefklas)
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


ggsave("06 output figuren/fig_respons_lft.svg", width = 12, height = 8)

# selectie totaal -
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
    color_pal = os_blauw[c(1, 4, 7)]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  ) +
  theme_os(orientation = 'horizontal')


# selectie totaal -
respons[["stadsdeel"]] |>
  filter(
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  mutate(
    gebied_stadsdeel_naam = replace_na(
      gebied_stadsdeel_naam,
      "buiten Amsterdam of buitenland"
    ),
    aandeel = aandeel * 100,
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
    fill = guide_legend(nrow = 2, reverse = T)
  ) +
  theme_os(orientation = 'horizontal')


ggsave("06 output figuren/fig_respons_sd.svg", width = 12, height = 8)
