# vraag 4: belangrijkste reden van bezoek

library(tidyverse)
library(openxlsx)

## script om my_selection en my_bind_rows in te lezen
#source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")
# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")


### vragen zijn opnieuw geformuleerd ---

data_26_bezoekers <- markt_list[["26_bez"]]

reden_bezoek <- data_26_bezoekers |>
  pivot_longer(cols = starts_with("v3_nw_")) |>
  mutate(name_tot = str_sub(name, end = -6)) |>
  filter(value %in% c("Yes", "No")) |>
  group_by(markt, name_tot, value) |>
  summarise(aantal = n()) |>
  group_by(markt, name_tot) |>
  mutate(aandeel = aantal / sum(aantal)) |>
  mutate(
    name_tot = case_when(
      name_tot == "v3_nw_anders_namelijk" ~ "anders",
      name_tot == "v3_nw_boodschappen_doe" ~ "boodschappen doen",
      name_tot == "v3_nw_eten_snacken_rea" ~ "eten/ snacken/ ready to eat",
      name_tot == "v3_nw_gewoon_een_leuk" ~ "gewoon een leuk uitje",
      name_tot == "v3_nw_gezelligheid_sfe" ~ "gezelligheid/sfeer op de markt",
      name_tot == "v3_nw_kwaliteit_van_he" ~ "kwaliteit van het productaanbod",
      name_tot == "v3_nw_lage_prijzen" ~ "lage prijzen",
      name_tot == "v3_nw_om_mensen_te_ont" ~ "om mensen te ontmoeten",
      name_tot == "v3_nw_toevallig" ~ "toevallig",
      name_tot == "v3_nw_variatie_in_het" ~ "variatie in het productaanbod",
      name_tot == "v3_nw_vast_praatje_mak" ~ "een vast praatje maken",
      name_tot == "v3_nw_wandeling_lunchp" ~ "wandeling/lunchpauze",
      name_tot == "v3_nw_weet_niet_geen_a" ~ "weet niet, geen antwoord"
    )
  ) |>
  filter(value == 'Yes')

reden_bezoek |>
  mutate(aandeel = aandeel * 100) |>
  filter(name_tot != 'weet niet, geen antwoord') |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_relevel(fct_reorder(name_tot, aandeel), "anders"),
    color_pal = os_blauw[7]
  ) +
  guides(
    color = 'none',
    fill = guide_legend(nrow = 2, reverse = T)
  ) +
  facet_wrap(~markt)


ggsave("06 output figuren/fig_v3_reden_bezoek.svg", width = 12, height = 6)
