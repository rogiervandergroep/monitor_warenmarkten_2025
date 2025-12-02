### vraag v5 komt u hier voornamelijk voor martkl of winkels

### deze vraag is gesteld voor alle jaargangen

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

# samenvoegen weesp en amsterdam
markt_22_def <-bind_rows(
  
  markt_list[["22_bez"]][["bez_22_ams"]]  |>
  select(v5, jaar, markt, leefklas, gebied_stadsdeel_naam),

  markt_list[["22_bez"]][["bez_22_wsp"]]  |>
  select(v5, jaar, markt, leefklas, gebied_stadsdeel_naam)
)


function_v5 <- function(x, group_vars) {
  bind_rows(
    markt_list[["16_bez"]] |>
      filter(
        v5 != 'niet ingevuld',
        !is.na(v5)
      ) |>
      group_by(jaar, v5, across(all_of(group_vars))) |>
      summarise(aantal = n()) |>
      group_by(jaar, across(all_of(group_vars))) |>
      mutate(aandeel = aantal / sum(aantal)),

    markt_22_def |>
      filter(
        v5 != 'niet ingevuld',
        !is.na(v5)
      ) |>
      group_by(jaar, v5, across(all_of(group_vars))) |>
      summarise(aantal = n()) |>
      group_by(jaar, across(all_of(group_vars))) |>
      mutate(aandeel = aantal / sum(aantal)),


    markt_list[["26_bez"]] |>
      filter(
        v5 != 'niet ingevuld',
        !is.na(v5)
      ) |>
      group_by(jaar, v5, across(all_of(group_vars))) |>
      summarise(aantal = n()) |>
      group_by(jaar, across(all_of(group_vars))) |>
      mutate(aandeel = aantal / sum(aantal))
  )
}

voornamelijk voor de markt                             
  
                     
anders namelijk                                        
weet niet, geen antwoord                               

       


tabel_v5 <- bind_rows(
  function_v5(group_vars = NULL) |>
    add_column(
      achtergrond_var = 'totaal',
      achtergrond_type = 'totaal'
    ),

  function_v5(group_vars = c("gebied_stadsdeel_naam")) |>
    add_column(achtergrond_var = 'gebied_stadsdeel_naam') |>
    rename('achtergrond_type' = 'gebied_stadsdeel_naam'),

  function_v5(group_vars = c("leefklas")) |>
    add_column(achtergrond_var = 'leefklas') |>
    rename('achtergrond_type' = 'leefklas'),

  function_v5(group_vars = c("markt")) |>
    add_column(achtergrond_var = 'markt') |>
    rename('achtergrond_type' = 'markt')
)|>
  mutate(v5 = case_when(
v5 == 'voornamelijk voor de winkels in dit <u>winkelgebied</u>' ~ 'voornamelijk voor de winkels in dit winkelgebied',
v5 == 'voornamleijk voor de supermarkten' ~ 'voornamelijk voor de supermarkten',
v5 == 'anders, namelijk' ~ 'anders',
TRUE ~ v5))



write.xlsx(tabel_v5, "05 output tabellen/tabel_v5_voornaamstedoel.xlsx")


####
source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_v5 |>
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
    fillvar = fct_reorder(v5, aandeel),
    color_pal = os_blauw
  )+
  facet_wrap(~ jaar)

ggsave("06 output figuren/fig_tabel_v5_sd.svg", width = 12, height = 6)




tabel_v5 |>

  filter(
    achtergrond_type != 'Pekmarkt',
    achtergrond_type != 'Zuidasmarkt',
    achtergrond_type != 'Westerstraat',
    achtergrond_type != 'Nieuwmarkt',
    achtergrond_type != 'Hermitagemarkt',
    !is.na(achtergrond_type),
    achtergrond_var %in% c('markt', 'totaal')
  ) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(achtergrond_type),
    fillvar = fct_reorder(v5, aandeel),
    color_pal = os_blauw
  )+
  facet_wrap(~ jaar)

ggsave("06 output figuren/fig_tabel_v5_markt.svg", width = 12, height = 6)

