# vraag 15 ondernemers
# Markt op afstand

library(tidyverse)
library(openxlsx)


## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")


# 2025: v15

function_v15 <- function(x, vraag, group_vars) {
  x |>
    filter(
      !is.na({{ vraag }}),
      {{ vraag }} != 'niet ingevuld'
    ) |>
    group_by({{ vraag }}, jaar, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    rename(v15 = {{ vraag }})
}

tabel_v15_ond <- list()

tabel_v15_ond[["totaal"]] <- markt_list[["26_ond"]] |>
  function_v15(vraag = v15, group_vars = NULL)

tabel_v15_ond[["markt"]] <- markt_list[["26_ond"]] |>
  function_v15(vraag = v15, group_vars = c("markt"))

tabel_v15_ond[["type_markt2"]] <- markt_list[["26_ond"]] |>
  function_v15(vraag = v15, group_vars = c("type_markt2"))

tabel_v15_ond[["stadsdeel"]] <- markt_list[["26_ond"]] |>
  function_v15(vraag = v15, group_vars = c("stadsdeel_markt"))

tabel_v15_ond[["leefklas"]] <- markt_list[["26_ond"]] |>
  function_v15(vraag = v15, group_vars = c("leefklas"))


write.xlsx(tabel_v15_ond, "05 output tabellen/tabel_v15_ond_opafstand.xlsx")
write_rds(tabel_v15_ond, "03 intermediate/tabel_v15_ond_opafstand.rds")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_v15_ond[['markt']] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(markt),
    fillvar = fct_rev(v15),
    color_pal = os_blauw[c(3, 5, 7)]
  )

ggsave("06 output figuren/fig_v15_ond_markt.svg", width = 12, height = 10)

bind_rows(
  tabel_v15_ond[['type_markt2']],
  tabel_v15_ond[['totaal']] |>
    add_column(type_markt2 = 'totaal')
) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(type_markt2),
    fillvar = fct_rev(v15),
    color_pal = os_blauw[c(3, 5, 7)],
    nr = 3
  )

ggsave("06 output figuren/fig_v15_ond_markt2.svg", width = 8, height = 5)
