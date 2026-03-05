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
  mutate(markt = str_trim(markt, "both")) |>
  mutate(markt = str_replace_all(markt, "Tussenmeer", "Tussen Meer"))


# koppeling met buurten en stadsdelen
functie_pc <- function(x, pcvar) {
  x |>
    mutate(postcode = str_to_upper(str_trim({{ pcvar }}, "both"))) |>
    left_join(data_pc_def, by = "postcode")
}


# toevoegen onbekend buitenland

functie_onbekend <- function(x, v_code) {
  x |>
    mutate(
      gebied_wijk_code = case_when(
        {{ v_code }} == 'respondent woont niet in Nederland' ~ 'buitenland',
        is.na(gebied_wijk_code) & postcode != "" ~ 'overig Nederland',
        TRUE ~ gebied_wijk_code
      )
    ) |>
    mutate(
      gebied_wijk_code = case_when(
        is.na(gebied_wijk_code) ~ 'woonplaats onbekend',
        TRUE ~ gebied_wijk_code
      )
    ) |>
    mutate(
      gebied_wijk_naam = case_when(
        is.na(gebied_wijk_naam) ~ gebied_wijk_code,
        TRUE ~ gebied_wijk_naam
      ),

      gebied_ggw_code = case_when(
        is.na(gebied_ggw_code) ~ gebied_wijk_code,
        TRUE ~ gebied_ggw_code
      ),

      gebied_ggw_naam = case_when(
        is.na(gebied_ggw_naam) ~ gebied_wijk_code,
        TRUE ~ gebied_ggw_naam
      ),

      gebied_stadsdeel_code = case_when(
        is.na(gebied_stadsdeel_code) ~ gebied_wijk_code,
        TRUE ~ gebied_stadsdeel_code
      ),

      gebied_stadsdeel_naam = case_when(
        is.na(gebied_stadsdeel_naam) ~ gebied_wijk_code,
        TRUE ~ gebied_stadsdeel_naam
      )
    )
}


# data 2016
markt_16_bez <- markt_16_bez |>
  functie_pc(pttkod) |>
  functie_onbekend(v_code = v17_codes)

markt_16_pas <- markt_16_pas |>
  functie_pc(pttkod) |>
  functie_onbekend(v_code = v17_codes)

# data 2022
markt_22_pas <- markt_22_pas |>
  map(\(x) functie_pc(x, v17)) |>
  map(\(x) functie_onbekend(x, v_code = v17_codes))


markt_22_bez <- markt_22_bez |>
  map(\(x) functie_pc(x, v19)) |>
  map(\(x) functie_onbekend(x, v_code = v19_codes))

# data 2026
markt_26_bez <- markt_26_bez |>
  functie_pc(v18) |>
  functie_onbekend(v_code = v18_codes)


# hernoemen marktnamen

my_markt_naam_mutate <- function(x, marktvar) {
  x |>
    rename(markt = {{ marktvar }}) |>
    mutate(
      markt = case_when(
        markt == 'Tussenmeer' ~ 'Tussen Meer',
        markt == "Plein '40 - '45" ~ "Plein '40-'45",
        markt == 'Plein 40-45' ~ "Plein '40-'45",
        markt == "Waterlooplein" ~ "Waterloopleinmarkt",
        markt == 'Biomarkt Zeeburg (van Eesterenlaan)' ~ 'Biomarkt Zeeburg',
        TRUE ~ markt
      )
    ) |>
    filter(
      markt != 'Westerstraat',
      markt != "19",
      markt != '18',
      markt != 'niet ingevuld'
    )
}


### hercoderen buurten in relatie tot markt ---

functie_rename_markt <- function(x) {
  x |>
    left_join(markt_locatie, by = "markt") |>
    mutate(
      type_markt2 = case_when(
        type_markt %in%
          c(
            "dagelijks",
            "meerdere dagen",
            "maandag, donderdag en vrijdag"
          ) ~ 'markt op meerdere dagen',
        TRUE ~ 'eendaagse markt'
      )
    ) |>
    mutate(
      locatie = case_when(
        gebied_stadsdeel_naam == 'buitenland' ~ 'buitenland',
        gebied_stadsdeel_naam == 'overig Nederland' ~ 'overig Nederland',
        gebied_stadsdeel_naam == 'woonplaats onbekend' ~ 'woonplaats onbekend',
        gebied_stadsdeel_naam ==
          stadsdeel_markt ~ 'woont in zelfde stadsdeel markt',
        gebied_stadsdeel_naam !=
          stadsdeel_markt ~ 'woont niet in zelfde stadsdeel markt'
      )
    )
}


functie_rename_markt_ond <- function(x, marktvar) {
  x |>
    left_join(markt_locatie, by = "markt") |>
    mutate(
      type_markt2 = case_when(
        type_markt %in%
          c(
            "dagelijks",
            "meerdere dagen",
            "maandag, donderdag en vrijdag"
          ) ~ 'markt op meerdere dagen',
        TRUE ~ 'eendaagse markt'
      )
    )
}


# uniforme naamgeving marktvariabele van b naar markt
# en alles in een list plaatsen

markt_list <- list()

markt_list[['16_bez']] <- markt_16_bez |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt()

markt_list[['16_pas']] <- markt_16_pas |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt()

markt_list[['16_ond']] <- markt_16_ond |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt_ond()


### voor 2022
markt_list[['22_bez']][["bez_22_ams"]] <- markt_22_bez[["bez_22_ams"]] |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt()

markt_list[['22_bez']][["bez_22_wsp"]] <- markt_22_bez[["bez_22_wsp"]] |>
  add_column(b = 'Weesp') |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt()


markt_list[['22_pas']][["pas_22_ams"]] <- markt_22_pas[["pas_22_ams"]] |>
  my_markt_naam_mutate(c) |>
  functie_rename_markt()

markt_list[['22_pas']][["pas_22_wsp"]] <- markt_22_pas[["pas_22_wsp"]] |>
  add_column(c = 'Weesp') |>
  my_markt_naam_mutate(c) |>
  functie_rename_markt()


markt_list[['22_ond']][["ond_22_ams"]] <- markt_22_ond[["ond_22_ams"]] |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt_ond()


markt_list[['22_ond']][["ond_22_wsp"]] <- markt_22_ond[["ond_22_wsp"]] |>
  add_column(b = 'Weesp') |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt_ond()


# voor 2026
markt_list[['26_bez']] <- markt_26_bez |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt()


markt_list[['26_ond']] <- markt_26_ond |>
  my_markt_naam_mutate(b) |>
  functie_rename_markt_ond()


write_rds(markt_list, "03 intermediate/markten_totaal.rds")
