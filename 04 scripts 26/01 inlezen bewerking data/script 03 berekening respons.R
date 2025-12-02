### berekening respons per markt ---

library(tidyverse)
library(openxlsx)

## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")

respons <- list()


respons[["totaal"]] <- my_bind_rows(groupvars = NULL) |>
  group_by(jaar, markt, groep) |>
  summarise(aantal = n()) |>
  pivot_wider(names_from = groep, values_from = aantal)

respons[["leeftijd"]] <- my_bind_rows(groupvars = NULL) |>
  group_by(jaar, markt, groep, leefklas) |>
  summarise(aantal = n()) |>
  pivot_wider(names_from = groep, values_from = aantal)


respons[["locatie"]] <- my_bind_rows(groupvars = NULL) |>
  group_by(jaar, markt, groep, locatie) |>
  summarise(aantal = n()) |>
  pivot_wider(names_from = groep, values_from = aantal)


write.xlsx(respons, "05 output tabellen/tabel_respons_overzicht.xlsx")
