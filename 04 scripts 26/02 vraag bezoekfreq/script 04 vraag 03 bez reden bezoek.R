# vraag 4: belangrijkste reden van bezoek

library(tidyverse)
library(openxlsx)

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")

# inlezen ruwe data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

v3_2016 <- markt_list[["16_bez"]] |>
  select(v301:v3_13) |>
  names()

v3_2022_ams <- markt_list[["22_bez"]][["bez_22_ams"]] |>
  select(v301:v313) |>
  names()

v3_2022_wsp <- markt_list[["22_bez"]][["bez_22_wsp"]] |>
  select(v301:v313) |>
  names()

markt_list[["22_bez_ams_wsp"]] <- bind_rows(
  markt_list[["22_bez"]][["bez_22_ams"]] |>
    select(all_of(v3_2022_ams), jaar, markt, type_markt2, leefklas),
  markt_list[["22_bez"]][["bez_22_wsp"]] |>
    select(all_of(v3_2022_wsp), jaar, markt, type_markt2, leefklas) |>
    set_names(c(v3_2022_ams, "jaar", "markt", "type_markt2", "leefklas"))
)

###############################
### in 2026 nieuwe naamgeving -
###############################

my_mutate <- function(x) {
  x |>
    mutate(
      name_tot = case_when(
        name_tot == "v3_nw_anders_namelijk" ~ "anders",
        name_tot == "v3_nw_boodschappen_doe" ~ "boodschappen doen",
        name_tot == "v3_nw_eten_snacken_rea" ~ "eten/ snacken/ ready to eat",
        name_tot == "v3_nw_gewoon_een_leuk" ~ "gewoon een leuk uitje",
        name_tot == "v3_nw_gezelligheid_sfe" ~ "gezelligheid/sfeer op de markt",
        name_tot ==
          "v3_nw_kwaliteit_van_he" ~ "kwaliteit van het productaanbod",
        name_tot == "v3_nw_lage_prijzen" ~ "lage prijzen",
        name_tot == "v3_nw_om_mensen_te_ont" ~ "om mensen te ontmoeten",
        name_tot == "v3_nw_toevallig" ~ "toevallig",
        name_tot == "v3_nw_variatie_in_het" ~ "variatie in het productaanbod",
        name_tot == "v3_nw_vast_praatje_mak" ~ "een vast praatje maken",
        name_tot == "v3_nw_wandeling_lunchp" ~ "wandeling/lunchpauze",
        name_tot == "v3_nw_weet_niet_geen_a" ~ "weet niet, geen antwoord"
      )
    )
}

# reden bezoek per markt 2026
reden_26_markt_bez <- markt_list[["26_bez"]] |>
  pivot_longer(cols = starts_with("v3_nw_")) |>
  mutate(name_tot = str_sub(name, end = -6)) |>
  filter(value %in% c("Yes", "No")) |>
  group_by(markt, name_tot, value) |>
  summarise(aantal = n()) |>
  group_by(markt, name_tot) |>
  mutate(aandeel = aantal / sum(aantal)) |>
  my_mutate() |>
  filter(value == 'Yes')


### open antwoorden bij "anders"
reden_26_markt_anders <- markt_list[["26_bez"]] |>
  pivot_longer(cols = starts_with("v3_nw_")) |>
  filter(name == 'v3_nw_anders_namelijk_gv1') |>
  group_by(markt, value) |>
  summarise(aantal = n()) |>
  filter(value != '') |>
  write_rds("03 intermediate/markten_v3_redenbezoek_anders.rds")


# reden bezoek per totaal
reden_26_totaal_bez <- markt_list[["26_bez"]] |>
  pivot_longer(cols = starts_with("v3_nw_")) |>
  mutate(name_tot = str_sub(name, end = -6)) |>
  filter(value %in% c("Yes", "No")) |>
  group_by(name_tot, value) |>
  summarise(aantal = n()) |>
  group_by(name_tot) |>
  mutate(aandeel = aantal / sum(aantal)) |>
  my_mutate() |>
  filter(value == 'Yes')


function_vraag <- function(x, y = x, i, group_vars) {
  labels <- y |>
    select(all_of(i)) |>
    names() |>
    map_df(
      \(i) {
        tibble(
          name = i,
          labels = attr(y[[i]], "label")
        )
      }
    )

  x |>
    pivot_longer(i) |>
    group_by(jaar, name, value, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    filter(!is.na(value)) |>
    left_join(labels, by = c('name')) |>
    group_by(jaar, name, across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    ungroup() |>
    select(-name)
}

v3_redenbezoek <- list()

# toaal
v3_redenbezoek[["totaal"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_vraag(
      i = v3_2016,
      group_vars = NULL
    ),
  markt_list[["22_bez_ams_wsp"]] |>
    function_vraag(
      y = markt_list[["22_bez"]][["bez_22_ams"]],
      i = v3_2022_ams,
      group_vars = NULL
    )
)

# naar markt
v3_redenbezoek[["markt"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_vraag(
      i = v3_2016,
      group_vars = c("markt")
    ),

  markt_list[["22_bez_ams_wsp"]] |>
    function_vraag(
      y = markt_list[["22_bez"]][["bez_22_ams"]],
      i = v3_2022_ams,
      group_vars = c("markt")
    )
)

# naar leeftijd
v3_redenbezoek[["leefklas"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_vraag(
      i = v3_2016,
      group_vars = c("leefklas")
    ),

  markt_list[["22_bez_ams_wsp"]] |>
    function_vraag(
      y = markt_list[["22_bez"]][["bez_22_ams"]],
      i = v3_2022_ams,
      group_vars = c("leefklas")
    )
)

write.xlsx(v3_redenbezoek_16_22, "05 output tabellen/tabel_v3redenbezoek.xlsx")


### vragen zijn opnieuw geformuleerd ---

reden_26_totaal_bez |>

  fun_totaal_een(
    xvar = aandeel * 100,
    yvar = fct_relevel(fct_reorder(name_tot, aandeel), "anders")
  )


ggsave(
  "06 output figuren/fig_v3_reden_bezoek_totaal.svg",
  width = 6,
  height = 6
)


# open antwoorden bij wat er precies gezeelig is

v3_gezellig_open <- markt_list[["26_bez"]] |>
  select(markt, v3a1_other0, v3a1_other1, v3a1_other2) |>
  pivot_longer(cols = starts_with("v3a1_")) |>
  group_by(markt, value) |>
  summarise(aantal = n()) |>
  filter(value != '') |>
  write_rds("03 intermediate/markten_v3_redenbezoek_gezellig.rds")
