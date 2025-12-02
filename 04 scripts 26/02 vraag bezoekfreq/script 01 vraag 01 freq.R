# vraag 1 (bezoekers en passanten): Hoe vaak bezoekt u deze markt?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")

### 2016 en 2022 V1 Hoe vaak bezoekt u deze markt? ---
### 2026 V2 Hoe vaak bezoekt u deze markt

markt_list[["26_bez"]] <- markt_list[["26_bez"]] |>
  select(-v1) |>
  mutate(v1 = v2)


df_markt_v1 <- my_bind_rows(groupvars = "v1") |>
  mutate(
    v1 = case_when(
      v1 %in%
        c(
          'zelden (ENQ: hier valt eenmalig ook onder!)',
          'zelden',
          'zelden tot nooit',
          'zelden (ENQ: hier valt eenmalig ook onder)'
        ) ~ 'zelden',
      v1 %in%
        c(
          'minder dan 1 keer per maand',
          'minder dan 1 keer maand'
        ) ~ 'minder dan 1 keer per maand',
      is.na(v1) ~ 'weet niet, geen antwoord',
      v1 == 'niet ingevuld' ~ 'weet niet, geen antwoord',
      TRUE ~ v1
    )
  ) |>
  filter(
    markt != "19",
    markt != '18',
    markt != 'niet ingevuld'
  ) |>
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
    v1 = case_when(
      (type_markt2 == 'eendaagse markt' &
        v1 == 'een aantal keer per week') ~ '1 keer per week',
      TRUE ~ v1
    )
  )


freq_levels_bez <- c(
  "een aantal keer per week",
  "1 keer per week",
  "1 keer per twee weken",
  "1 keer per maand",
  "minder dan 1 keer per maand",
  "zelden",
  "weet niet, geen antwoord"
)

freq_levels_ond <- c(
  "korter dan 1 jaar",
  "1 tot en met 2 jaar",
  "3 tot en met 5 jaar",
  "6 tot en met 10 jaar",
  "langer dan 10 jaar",
  "weet niet, geen antwoord"
)


###################################################

# tabel funtie -
functie_tabel <- function(x, group_vars) {
  x |>
    group_by(jaar, groep, v1, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(jaar, groep, across(all_of(group_vars))) |>
    mutate(perc = aantal / sum(aantal) * 100)
}


### totalen:
tabel_v1 <- bind_rows(
  # freq totaal
  df_markt_v1 |>
    functie_tabel(group_vars = NULL) |>
    add_column(type_markt2 = 'totaal', markt = 'totaal') |>
    add_column(leefklas = 'totaal', locatie = 'totaal'),

  # freq naar type markt
  df_markt_v1 |>
    functie_tabel(group_vars = c("type_markt2")) |>
    add_column(markt = 'totaal') |>
    add_column(leefklas = 'totaal', locatie = 'totaal'),

  # freq naar markt
  df_markt_v1 |>
    functie_tabel(group_vars = c("markt", "type_markt2")) |>
    add_column(leefklas = 'totaal', locatie = 'totaal'),

  # naar leeftijd -
  df_markt_v1 |>
    functie_tabel(group_vars = c("leefklas")) |>
    add_column(markt = 'totaal', type_markt2 = 'totaal'),

  df_markt_v1 |>
    functie_tabel(group_vars = c("markt", "leefklas")) |>
    add_column(type_markt2 = 'totaal'),

  # naar woonstadsdeel -
  df_markt_v1 |>
    functie_tabel(group_vars = c("locatie")) |>
    add_column(markt = 'totaal', type_markt2 = 'totaal'),

  df_markt_v1 |>
    functie_tabel(group_vars = c("markt", "locatie")) |>
    add_column(type_markt2 = 'totaal')
)

write.xlsx(tabel_v1, "05 output tabellen/tabel_v1_frequentie.xlsx")
write_rds(tabel_v1, "03 intermediate/markten_v1_freq.rds")
