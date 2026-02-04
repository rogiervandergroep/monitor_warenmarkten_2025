# vraag 4 ondernemers
# Is het aantal bezoekers op de markt in de afgelopen 3 jaar volgens u toe-  of afgenomen?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

# 2016: v4a
# 2022: v4a
# 2025: v4a

function_toename <- function(x, vraag, group_vars) {
  x |>
    filter(
      !is.na({{ vraag }}),
      {{ vraag }} != 'niet ingevuld'
    ) |>
    group_by({{ vraag }}, jaar, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    rename(v4a = {{ vraag }})
}


### samenvoegen data 2022

markt_list[["22_ond"]] <- bind_rows(
  markt_list[["22_ond"]][["ond_22_ams"]] |>
    select(v4a, jaar, markt, type_markt2, leefklas),
  markt_list[["22_ond"]][["ond_22_wsp"]] |>
    select(v4a, jaar, markt, type_markt2, leefklas)
)


tabel_v4a <- list()

tabel_v4a[["totaal"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v4a, group_vars = NULL),
  markt_list[["22_ond"]] |>
    function_toename(vraag = v4a, group_vars = NULL),
  markt_list[["26_ond"]] |>
    function_toename(vraag = v4a, group_vars = NULL)
)

### markt
tabel_v4a[["markt"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("markt")),
  markt_list[["22_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("markt")),
  markt_list[["26_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("markt"))
)

tabel_v4a[["leefklas"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("leefklas")),

  markt_list[["22_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("leefklas")),

  markt_list[["26_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("leefklas"))
) |>
  mutate(leefklas = replace_na(leefklas, "leeftijd onbekend"))


tabel_v4a[["type_markt2"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("type_markt2")),

  markt_list[["22_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("type_markt2")),

  markt_list[["26_ond"]] |>
    function_toename(vraag = v4a, group_vars = c("type_markt2"))
)

write.xlsx(tabel_v4a, "05 output tabellen/tabel_v4a_ond_voorachter.xlsx")
write_rds(tabel_v4a, "03 intermediate/tabel_v4a_ond_voorachter.rds")

# vb:

tab_reden_4b <- markt_list[["26_ond"]] |>
  select(markt, v4a, v4b) |>
  mutate(
    v4a = case_when(
      v4a == 'sterk toegenomen' ~ 'toegenomen',
      v4a == 'sterk afgenomen' ~ 'afgenomen',
      TRUE ~ v4a
    )
  ) |>
  group_by(markt, v4a) |>
  summarise(v4b_reden = paste(v4b, collapse = "; ")) |>
  filter(
    !is.na(v4a),
    v4a != 'stabiel'
  )

write_rds(tab_reden_4b, "07 quarto/03 data/tab_reden_on_4b.rds")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_v4a[["totaal"]] |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(jaar),
    fillvar = fct_rev(v4a),
    color_pal = os_blauw[c(1, 3, 4, 6, 7)]
  )

ggsave("06 output figuren/fig_v4a_ond_totaal.svg", width = 12, height = 6)


tabel_v4a[['markt']] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(markt),
    fillvar = fct_rev(v4a),
    color_pal = os_blauw[c(1, 3, 4, 6, 7)]
  )

ggsave("06 output figuren/fig_v4a_ond_markt.svg", width = 12, height = 10)

bind_rows(
  tabel_v4a[['type_markt2']],
  tabel_v4a[['totaal']] |>
    add_column(type_markt2 = 'totaal')
) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(jaar),
    fillvar = fct_rev(v4a),
    color_pal = os_blauw
  ) +
  facet_wrap(~type_markt2)

ggsave("06 output figuren/fig_v4a_ond_markt2.svg", width = 12, height = 5)

### toevoegen open antwoorden ---
