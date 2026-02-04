# vraag 4: voor welke producten komt u naar de markt

library(tidyverse)
library(openxlsx)

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")

# inlezen ruwe data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

# producten die op de markt gekocht worden

labels <- markt_list[["26_bez"]] |>
  select(v401:v417) |>
  names() |>
  map_df(
    \(i) {
      tibble(
        name = i,
        labels = attr(markt_list[["26_bez"]][[i]], "label")
      )
    }
  ) |>
  mutate(
    labels = case_when(
      name == 'v407' ~ 'andere etenswaren',
      name == 'v408' ~ 'kant-en-klaar voedsel',
      name == 'v412' ~ 'huishoudelijke artikelen',
      name == 'v414' ~ 'media',
      name == 'v416' ~ 'anders',
      TRUE ~ labels
    )
  )


tab_v4_producten_markt <- bind_rows(
  markt_list[["26_bez"]] |>
    pivot_longer(cols = c(v401:v417)) |>
    filter(value %in% c("Yes", "No")) |>
    group_by(markt, name, value) |>
    summarise(aantal = n()) |>
    group_by(markt, name) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    filter(value == 'Yes') |>
    left_join(labels, by = "name"),

  markt_list[["26_bez"]] |>
    pivot_longer(cols = c(v401:v417)) |>
    filter(value %in% c("Yes", "No")) |>
    group_by(name, value) |>
    summarise(aantal = n()) |>
    group_by(name) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    filter(value == 'Yes') |>
    left_join(labels, by = "name") |>
    add_column(markt = 'totaal')
) |>
  write_rds("03 intermediate/tab_markten_v4_prod.rds")


### open antwoorden bij "anders"
tab_v4_producten_markt_anders <- markt_list[["26_bez"]] |>
  select(markt, v4_other15) |>
  filter(v4_other15 != '') |>
  write_rds("03 intermediate/tab_markten_v4_prod_anders.rds")


### vragen zijn opnieuw geformuleerd ---

tab_v4_producten_markt |>
  filter(markt == 'totaal') |>

  fun_totaal_een(
    xvar = aandeel,
    yvar = fct_relevel(
      fct_reorder(labels, aandeel),
      "weet niet, geen antwoord",
      "anders"
    )
  )


ggsave(
  "06 output figuren/fig_v4_gekochte_producten_totaal.svg",
  width = 6,
  height = 6
)
