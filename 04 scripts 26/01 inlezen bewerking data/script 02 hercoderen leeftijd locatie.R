### hercoderen  leeftijd
### toevoegen buurten
### eenduidige variabelnamen

library(tidyverse)
library(openxlsx)

# inlezen data -
source("04 scripts 26/01 inlezen bewerking data/script 01 inlezen data.R")


functie_leeftijd <- function(data, leeftijd) {
  data |>
    mutate(
      leefklas = case_when(
        {{ leeftijd }} < 35 ~ 'jonger dan 35 jaar',
        {{ leeftijd }} %in% c(35:55) ~ 'tussen 35 en 55 jaar',
        {{ leeftijd }} %in% c(56:67) ~ 'tussen 56 en 67 jaar',
        {{ leeftijd }} > 68 ~ '68 jaar en ouder'
      )
    )
}

# nb: in 2016 is gevraagd naar leeftijd
markt_16_bez <- functie_leeftijd(markt_16_bez, v16)
markt_16_pas <- functie_leeftijd(markt_16_pas, v16)
markt_16_ond <- functie_leeftijd(markt_16_ond, v16)

# nb: in 2022 en 2025 is gevraagd naar geboortejaar

# data 2022 (in een list, want aangevuld met veldwerk weesp)
markt_22_bez <- markt_22_bez |>
  map(\(x) mutate(x, v18n = 2022 - v18)) |>
  map(\(x) functie_leeftijd(x, v18n))

markt_22_ond <- markt_22_ond |>
  map(\(x) mutate(x, v16n = 2022 - v16)) |>
  map(\(x) functie_leeftijd(x, v16n))

markt_22_pas <- markt_22_pas |>
  map(\(x) mutate(x, v16n = 2022 - v16)) |>
  map(\(x) functie_leeftijd(x, v16n))

# data 2026 (veldwerk vond plaats in 2025)
markt_26_bez <- markt_26_bez |>
  mutate(v15n = 2025 - v15) |>
  functie_leeftijd(v15n)

markt_26_ond <- markt_26_ond |>
  mutate(v17n = 2025 - v17) |>
  functie_leeftijd(v17n)


# postcode en locatie markt
data_pc6 <- read.csv(
  "02 references/postcode6 2025.csv"
) |>
  mutate(gebied_wijk_code = str_replace_na(gebied_wijk_code, "NA"))


# omdat sommige 4-cijferige postcodes in meerdere wijken vallen is gekozen voor de meest voorkomende wijk
data_pc4 <- data_pc6 |>
  mutate(postcode = str_sub(postcode, 1, 4)) |>
  group_by(postcode, gebied_wijk_code) |>
  mutate(aantal = n()) |>
  distinct() |>
  group_by(postcode) |>
  filter(aantal == max(aantal)) |>
  select(-aantal)

data_pc_def <- bind_rows(data_pc6, data_pc4)

markt_locatie <- openxlsx::read.xlsx(
  "02 references/Lookup_B_marktlocaties.xlsx"
) |>
  mutate(markt = str_trim(markt, "both"))


# koppeling met buurten en stadsdelen
functie_pc <- function(x, pcvar) {
  x |>
    mutate(postcode = str_to_upper(str_trim({{ pcvar }}, "both"))) |>
    left_join(data_pc_def, by = "postcode")
}

# data 2016
markt_16_bez <- markt_16_bez |>
  functie_pc(pttkod)

markt_16_pas <- markt_16_pas |>
  functie_pc(pttkod)

# data 2022
markt_22_pas <- markt_22_pas |>
  map(\(x) functie_pc(x, v17))
markt_22_bez <- markt_22_bez |>
  map(\(x) functie_pc(x, v19))

# data 2026
markt_26_bez <- markt_26_bez |>
  functie_pc(v18)


### hercoderen buurten in relatie tot markt ---

functie_rename_markt <- function(x, marktvar) {
  x |>
    rename(markt = {{ marktvar }}) |>
    left_join(markt_locatie, by = "markt") |>
    mutate(
      locatie = case_when(
        gebied_stadsdeel_naam ==
          stadsdeel_markt ~ 'woont in zelfde stadsdeel markt',
        gebied_stadsdeel_naam !=
          stadsdeel_markt ~ 'woont niet in zelfde stadsdeel markt',
        is.na(
          gebied_stadsdeel_naam
        ) ~ 'woont buiten Amsterdam of woonplaats onbekend'
      )
    )
}

functie_rename_markt_ond <- function(x, marktvar) {
  x |>
    rename(markt = {{ marktvar }}) |>
    left_join(markt_locatie, by = "markt")
}


# uniforme naamgeving marktvariabele van b naar markt
# en alles in een list plaatsen

# hernoemen martknamen

my_markt_naam_mutate <- function(x) {
  x |>
    mutate(
      markt = case_when(
        markt == "Plein '40 - '45" ~ "Plein 40-45",
        markt == "Waterlooplein" ~ "Waterloopleinmarkt",
        markt == 'Biomarkt Zeeburg (van Eesterenlaan)' ~ 'Biomarkt Zeeburg',
        TRUE ~ markt
      )
    )
}

markt_list <- list()

markt_list[['16_bez']] <- markt_16_bez |>
  functie_rename_markt(b) |>
  my_markt_naam_mutate()
markt_list[['16_pas']] <- markt_16_pas |>
  functie_rename_markt(b) |>
  my_markt_naam_mutate()
markt_list[['16_ond']] <- markt_16_ond |>
  functie_rename_markt_ond(b) |>
  my_markt_naam_mutate()


### voor 2022
markt_list[['22_bez']][["bez_22_ams"]] <- markt_22_bez[["bez_22_ams"]] |>
  functie_rename_markt(b) |>
  my_markt_naam_mutate()
markt_list[['22_bez']][["bez_22_wsp"]] <- markt_22_bez[["bez_22_wsp"]] |>
  add_column(b = 'Weesp') |>
  functie_rename_markt(b) |>
  my_markt_naam_mutate()

markt_list[['22_pas']][["pas_22_ams"]] <- markt_22_pas[["pas_22_ams"]] |>
  functie_rename_markt(c) |>
  my_markt_naam_mutate()
markt_list[['22_pas']][["pas_22_wsp"]] <- markt_22_pas[["pas_22_wsp"]] |>
  add_column(c = 'Weesp') |>
  functie_rename_markt(c) |>
  my_markt_naam_mutate()

markt_list[['22_ond']][["ond_22_ams"]] <- markt_22_ond[["ond_22_ams"]] |>
  functie_rename_markt_ond(b) |>
  my_markt_naam_mutate()
markt_list[['22_ond']][["ond_22_wsp"]] <- markt_22_ond[["ond_22_wsp"]] |>
  add_column(b = 'Weesp') |>
  functie_rename_markt_ond(b) |>
  my_markt_naam_mutate()


# voor 2026
markt_list[['26_bez']] <- markt_26_bez |>
  functie_rename_markt(b) |>
  my_markt_naam_mutate()
markt_list[['26_ond']] <- markt_26_ond |>
  functie_rename_markt_ond(b) |>
  my_markt_naam_mutate()


write_rds(markt_list, "03 intermediate/markten_totaal.rds")
