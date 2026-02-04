# vraag 16 ondernemers
# ziet u zich hier nog staan

library(tidyverse)
library(openxlsx)


## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")


# 2025: v16a

function_v16a <- function(x, vraag, group_vars) {
  x |>
    filter(
      !is.na({{ vraag }}),
      {{ vraag }} != 'niet ingevuld'
    ) |>
    group_by({{ vraag }}, jaar, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    rename(v16a = {{ vraag }})
}

tabel_v16a_ond <- list()

tabel_v16a_ond[["totaal"]] <- markt_list[["26_ond"]] |>
  function_v16a(vraag = v16a, group_vars = NULL)

tabel_v16a_ond[["markt"]] <- markt_list[["26_ond"]] |>
  function_v16a(vraag = v16a, group_vars = c("markt"))

tabel_v16a_ond[["type_markt2"]] <- markt_list[["26_ond"]] |>
  function_v16a(vraag = v16a, group_vars = c("type_markt2"))

tabel_v16a_ond[["stadsdeel"]] <- markt_list[["26_ond"]] |>
  function_v16a(vraag = v16a, group_vars = c("stadsdeel_markt"))

tabel_v16a_ond[["leefklas"]] <- markt_list[["26_ond"]] |>
  function_v16a(vraag = v16a, group_vars = c("leefklas"))


write.xlsx(tabel_v16a_ond, "05 output tabellen/tabel_v16a_ond_tienjaar.xlsx")
write_rds(tabel_v16a_ond, "03 intermediate/tabel_v16a_ond_tienjaar.rds")


### open antwoorden

# v10b toelichting wel of niet over tien jaar

tab_ond_v16_open <- markt_list[["26_ond"]] |>
  select(markt, v16a, v16b) |>
  mutate(
    v16a = case_when(
      grepl("ja", v16a) ~ "ja",
      grepl("nee", v16a) ~ "nee",
      TRUE ~ v16a
    )
  ) |>
  filter(
    !is.na(v16a),
    v16b != ''
  ) |>
  group_by(markt, v16a) |>
  summarise(v16_open = paste(v16b, collapse = "; "))

write_rds(tab_ond_v16_open, "07 quarto/03 data/tab_v16_open.rds")

# v11 wqat is het sterkste punt van de martk
tab_ond_v11_open <- markt_list[["26_ond"]] |>
  select(markt, v11) |>
  filter(v11 != '') |>
  group_by(markt) |>
  summarise(v11_open = paste(v11, collapse = "; "))

write_rds(tab_ond_v11_open, "07 quarto/03 data/tab_v11_sterk_open.rds")


# v12 wat is het verbeterpunt van de marekt
tab_ond_v12_open <- markt_list[["26_ond"]] |>
  select(markt, v12) |>
  filter(v12 != '') |>
  group_by(markt) |>
  summarise(v12_open = paste(v12, collapse = "; "))

write_rds(tab_ond_v12_open, "07 quarto/03 data/tab_v12_zwak_open.rds")


tab_ond_opm <- markt_list[["26_ond"]] |>
  select(markt, opmerkingen) |>
  filter(opmerkingen != '') |>
  group_by(markt) |>
  summarise(opmerkingen = paste(opmerkingen, collapse = "; "))
write_rds(tab_ond_opm, "07 quarto/03 data/tab_ond_opmerkingen.rds")

tab_bez_opm <- markt_list[["26_bez"]] |>
  select(markt, opmerkingen) |>
  filter(opmerkingen != '') |>
  group_by(markt) |>
  summarise(opmerkingen = paste(opmerkingen, collapse = "; "))
write_rds(tab_bez_opm, "07 quarto/03 data/tab_bez_opmerkingen.rds")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_v16a_ond[['markt']] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(markt),
    fillvar = fct_rev(v16a),
    color_pal = os_blauw[c(3, 2, 5, 7)]
  )

ggsave("06 output figuren/fig_v16a_ond_markt.svg", width = 12, height = 10)

bind_rows(
  tabel_v16a_ond[['type_markt2']],
  tabel_v16a_ond[['totaal']] |>
    add_column(type_markt2 = 'totaal')
) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(type_markt2),
    fillvar = fct_rev(v16a),
    color_pal = os_blauw[c(3, 2, 5, 7)],
    nr = 2
  )


bind_rows(
  tabel_v16a_ond[['leefklas']],
  tabel_v16a_ond[['totaal']] |>
    add_column(leefklas = 'totaal')
) |>
  filter(!is.na(leefklas)) |>
  mutate(leefklas = factor(leefklas, levels = levels_leefklas)) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(leefklas),
    fillvar = fct_rev(v16a),
    color_pal = os_blauw[c(3, 2, 5, 7)],
    nr = 2
  )


ggsave("06 output figuren/fig_v16a_ond_leefklas.svg", width = 8, height = 5)
