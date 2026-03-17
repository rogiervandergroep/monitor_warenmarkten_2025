# vraag 7 ondernemers
# als u deze markt vergelijkt ....

library(tidyverse)
library(openxlsx)


## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

# 2016: v7
# 2022: v7
# 2025: v7

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
    rename(v7 = {{ vraag }})
}


### samenvoegen data 2022

markt_list[["22_ond"]] <- bind_rows(
  markt_list[["22_ond"]][["ond_22_ams"]] |>
    select(v7, jaar, markt, type_markt2, leefklas),
  markt_list[["22_ond"]][["ond_22_wsp"]] |>
    select(v7, jaar, markt, type_markt2, leefklas)
)


tabel_v7 <- list()

tabel_v7[["totaal"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v7, group_vars = NULL),
  markt_list[["22_ond"]] |>
    function_toename(vraag = v7, group_vars = NULL),
  markt_list[["26_ond"]] |>
    function_toename(vraag = v7, group_vars = NULL)
)

### markt
tabel_v7[["markt"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v7, group_vars = c("markt")),
  markt_list[["22_ond"]] |>
    function_toename(vraag = v7, group_vars = c("markt")),
  markt_list[["26_ond"]] |>
    function_toename(vraag = v7, group_vars = c("markt"))
)

tabel_v7[["leefklas"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v7, group_vars = c("leefklas")),

  markt_list[["22_ond"]] |>
    function_toename(vraag = v7, group_vars = c("leefklas")),

  markt_list[["26_ond"]] |>
    function_toename(vraag = v7, group_vars = c("leefklas"))
) |>
  mutate(leefklas = replace_na(leefklas, "leeftijd onbekend"))


tabel_v7[["type_markt2"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_toename(vraag = v7, group_vars = c("type_markt2")),

  markt_list[["22_ond"]] |>
    function_toename(vraag = v7, group_vars = c("type_markt2")),

  markt_list[["26_ond"]] |>
    function_toename(vraag = v7, group_vars = c("type_markt2"))
)

write.xlsx(tabel_v7, "05 output tabellen/tabel_v7_ond_beterslechter.xlsx")
write_rds(tabel_v7, "03 intermediate/tabel_v7_ond_beterslechter.rds")


source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_v7[["totaal"]] |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(jaar),
    fillvar = fct_rev(v7),
    color_pal = os_blauw[c(1, 2, 3, 4, 6, 7)]
  )

ggsave("06 output figuren/fig_v7_ond_totaal.svg", width = 12, height = 6)


tabel_v7[['markt']] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(markt),
    fillvar = fct_rev(v7),
    color_pal = os_blauw[c(1, 2, 3, 4, 6, 7)]
  )

ggsave("06 output figuren/fig_v7_ond_markt.svg", width = 12, height = 10)

bind_rows(
  tabel_v7[['type_markt2']],
  tabel_v7[['totaal']] |>
    add_column(type_markt2 = 'totaal')
) |>
  fun_totaal(
    xvar = aandeel,
    yvar = fct_rev(jaar),
    fillvar = fct_rev(v7),
    color_pal = stoplicht6[c(1, 2, 3, 4, 6, 7)]
  ) +
  facet_wrap(~type_markt2)

ggsave("06 output figuren/fig_v7_ond_markt2.svg", width = 10, height = 4)

### toevoegen open antwoorden ---
