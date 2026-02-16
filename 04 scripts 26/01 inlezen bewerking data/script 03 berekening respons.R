### berekening respons per markt ---

library(tidyverse)
library(openxlsx)

## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")

source("04 scripts 26/00 scr/script 00 levels.R")

respons <- list()


respons[["totaal"]] <- my_bind_rows(groupvars = c("type_markt2")) |>
  group_by(jaar, type_markt2, markt, groep) |>
  summarise(aantal = n()) |>
  pivot_wider(names_from = groep, values_from = aantal)

respons[["stadsdeel"]] <- my_bind_rows(
  groupvars = c("gebied_stadsdeel_code", "gebied_stadsdeel_naam")
) |>
  group_by(jaar, groep, gebied_stadsdeel_code, gebied_stadsdeel_naam) |>
  summarise(aantal = n()) |>
  group_by(jaar, groep) |>
  mutate(aandeel = aantal / sum(aantal)) |>
  mutate(
    gebied_stadsdeel_naam = replace_na(
      gebied_stadsdeel_naam,
      "buiten Amsterdam of buitenland"
    ),
    gebied_stadsdeel_code = replace_na(gebied_stadsdeel_code, "Z")
  )

respons[["leeftijd"]] <- bind_rows(
  my_bind_rows(groupvars = c("type_markt2")) |>
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
) |>
  mutate(
    leefklas = replace_na(leefklas, "leeftijd onbekend"),
    leefklas = factor(leefklas, levels = levels_leefklas)
  )

respons[["locatie"]] <- bind_rows(
  my_bind_rows(groupvars = c("type_markt2")) |>
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

### hercoderen huishoudsituatie en bezigheid

markt_list[["26_bez"]] <- markt_list[["26_bez"]] |>
  mutate(
    thuissituatie = case_when(
      v17 == "eenpersoonshuishouden" ~ "eenpersoonshuishouden",
      v17 == "(echt)paar zonder kinderen thuis" ~ "paar zonder kinderen",
      v17 == "(echt)paar met kind(eren) thuis" ~ "paar met kinderen",
      v17 == "één ouder met kind(eren) thuis" ~ "eenouderhuishouden",
      TRUE ~ "anders of onbekend"
    )
  )

# naar huishoudsituatie
respons[["huishoudsituatie"]] <- bind_rows(
  markt_list[["26_bez"]] |>
    group_by(jaar, type_markt2, markt, groep, thuissituatie) |>
    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt, groep) |>
    mutate(aandeel = aantal / sum(aantal)),

  markt_list[["26_bez"]] |>
    group_by(jaar, groep, thuissituatie) |>
    summarise(aantal = n()) |>
    group_by(jaar, groep) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
)


# naar arbeidsmarktrelatie
respons[["arbeidsmarkt"]] <- bind_rows(
  markt_list[["26_bez"]] |>
    group_by(jaar, type_markt2, markt, groep, v16) |>
    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt, groep) |>
    mutate(aandeel = aantal / sum(aantal)),

  markt_list[["26_bez"]] |>
    group_by(jaar, groep, v16) |>
    summarise(aantal = n()) |>
    group_by(jaar, groep) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
)


### kenmerken ondernemers ---

# vraag 1 bij ondernemers hoe lang staat u op de markt

respons[["ond_lengte"]] <- bind_rows(
  my_bind_rows(groupvars = c("type_markt2", "v1")) |>
    mutate(
      v1 = str_replace_all(
        v1,
        "niet ingevuld",
        "weet niet, geen antwoord"
      )
    ) |>
    filter(groep == 'ondernemers') |>
    group_by(jaar, type_markt2, markt, v1) |>
    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt) |>
    mutate(aandeel = aantal / sum(aantal)),

  my_bind_rows(groupvars = c("v1")) |>
    mutate(
      v1 = str_replace_all(
        v1,
        "niet ingevuld",
        "weet niet, geen antwoord"
      )
    ) |>
    filter(groep == "ondernemers") |>
    group_by(jaar, v1) |>
    summarise(aantal = n()) |>
    group_by(jaar) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
) |>
  mutate(v1 = factor(v1, levels = levels_ond_lengte))


# vraag 2 vaste plek of sollicitant
respons[["ond_plek"]] <- bind_rows(
  my_bind_rows(groupvars = c("type_markt2", "v2")) |>
    filter(groep == 'ondernemers') |>
    mutate(
      v2 = case_when(
        v2 == 'sollicitant' ~ 'sollicitant / loteling',
        v2 == 'niet ingevuld' ~ "weet niet, geen antwoord",
        is.na(v2) ~ 'weet niet, geen antwoord',
        TRUE ~ v2
      )
    ) |>
    group_by(jaar, type_markt2, markt, v2) |>
    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt) |>
    mutate(aandeel = aantal / sum(aantal)),

  my_bind_rows(groupvars = c("v2")) |>
    filter(groep == "ondernemers") |>
    mutate(
      v2 = case_when(
        v2 == 'sollicitant' ~ 'sollicitant / loteling',
        v2 == 'niet ingevuld' ~ "weet niet, geen antwoord",
        is.na(v2) ~ 'weet niet, geen antwoord',
        TRUE ~ v2
      )
    ) |>
    group_by(jaar, v2) |>
    summarise(aantal = n()) |>
    group_by(jaar) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
) |>
  mutate(
    v2 = factor(
      v2,
      levels = c(
        "vergunninghouder",
        "sollicitant / loteling",
        "weet niet, geen antwoord"
      )
    )
  )


# vraag 3 verkoop food non food

respons[["ond_verkoop"]] <- bind_rows(
  my_bind_rows(groupvars = c("type_markt2", "v3")) |>
    filter(groep == 'ondernemers') |>
    mutate(
      v3 = case_when(
        v3 == 'food, namelijk' ~ 'food',
        v3 == 'non food, namelijk' ~ 'non food',
        v3 == 'non food , namelijk' ~ 'non food',
        v3 == 'niet ingevuld' ~ "weet niet, geen antwoord",
        is.na(v3) ~ 'weet niet, geen antwoord',
        TRUE ~ v3
      )
    ) |>
    group_by(jaar, type_markt2, markt, v3) |>

    summarise(aantal = n()) |>
    group_by(jaar, type_markt2, markt) |>
    mutate(aandeel = aantal / sum(aantal)),

  my_bind_rows(groupvars = c("v3")) |>
    filter(groep == "ondernemers") |>
    mutate(
      v3 = case_when(
        v3 == 'food, namelijk' ~ 'food',
        v3 == 'non food, namelijk' ~ 'non food',
        v3 == 'non food , namelijk' ~ 'non food',
        v3 == 'niet ingevuld' ~ "weet niet, geen antwoord",
        is.na(v3) ~ 'weet niet, geen antwoord',
        TRUE ~ v3
      )
    ) |>
    group_by(jaar, v3) |>
    summarise(aantal = n()) |>
    group_by(jaar) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    add_column(
      type_markt2 = 'totaal',
      markt = 'totaal'
    )
) |>
  mutate(
    v3 = factor(
      v3,
      levels = c("food", "non food", "weet niet, geen antwoord")
    )
  )


#### open antwoorden ---
respons[["ond_food"]] <- my_bind_rows(
  groupvars = c("type_markt2", "v3", "v3_food")
) |>
  filter(groep == 'ondernemers') |>
  group_by(jaar, type_markt2, markt, v3, v3_food) |>
  summarise(aantal = n())

# open antwoorden food
respons[["ond_nonfood"]] <- my_bind_rows(
  groupvars = c("type_markt2", "v3", "v3_non_food")
) |>
  filter(groep == 'ondernemers') |>
  group_by(jaar, type_markt2, markt, v3, v3_non_food) |>
  summarise(aantal = n())


write.xlsx(respons, "05 output tabellen/tabel_respons_overzicht.xlsx")

# voor plots in rapport
write_rds(respons, "03 intermediate/markten_respons.rds")

# voor plots in factsheet
write_rds(respons, "07 quarto/01 intermediate/markten_respons.rds")
