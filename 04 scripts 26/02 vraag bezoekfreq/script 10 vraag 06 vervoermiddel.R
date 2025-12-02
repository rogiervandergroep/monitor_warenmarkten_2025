### vraag 6 hoe bent u vandaag naar de markt gereisd
### alleen gesteld in 2026?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")


function_v6 <- function(group_vars) {
  markt_list[["26_bez"]] |>
    filter(!is.na(v6)) |>
    mutate(
      v6 = case_when(
        v6 == 'anders, namelijk' ~ 'anders',
        TRUE ~ v6
      )
    ) |>
    group_by(v6, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal))
}


tabel_v6 <- bind_rows(
  function_v6(group_vars = NULL) |>
    add_column(
      achtergrond_var = 'totaal',
      achtergrond_type = 'totaal'
    ),

  function_v6(group_vars = c("gebied_stadsdeel_naam")) |>
    add_column(achtergrond_var = 'gebied_stadsdeel_naam') |>
    rename('achtergrond_type' = 'gebied_stadsdeel_naam'),

  function_v6(group_vars = c("leefklas")) |>
    add_column(achtergrond_var = 'leefklas') |>
    rename('achtergrond_type' = 'leefklas'),

  function_v6(group_vars = c("locatie")) |>
    add_column(achtergrond_var = 'locatie') |>
    rename('achtergrond_type' = 'locatie'),

  function_v6(group_vars = c("markt")) |>
    add_column(achtergrond_var = 'markt') |>
    rename('achtergrond_type' = 'markt')
)

write.xlsx(tabel_v6, "05 output tabellen/tabel_v6_vervoermiddel.xlsx")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_v6 |>
  filter(
    !is.na(achtergrond_type),
    achtergrond_type != 'Westpoort',
    achtergrond_var %in% c('gebied_stadsdeel_naam', 'totaal')
  ) |>
  mutate(
    achtergrond_type = factor(achtergrond_type, levels = levels_stadsdeel)
  ) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(achtergrond_type),
    fillvar = fct_reorder(v6, aandeel),
    color_pal = os_blauw
  )

ggsave("06 output figuren/fig_tabel_v6_sd.svg", width = 12, height = 6)


tabel_v6 |>
  filter(
    !is.na(achtergrond_type),
    achtergrond_var == 'markt'
  ) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(achtergrond_type),
    fillvar = fct_reorder(v6, aandeel),
    color_pal = os_blauw
  )

ggsave("06 output figuren/fig_tabel_v6_markt.svg", width = 12, height = 6)
