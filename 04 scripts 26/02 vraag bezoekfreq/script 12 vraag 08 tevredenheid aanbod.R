### vraag 8 en 9  over
# 7 of 8. Hoe vindt u het aanbod van artikelenen producten op deze markt?
# 9. Waarom bent u ontevreden over het productaanbod?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

# 2016: v7
# 2022: v7
# 2025: v8

function_tevr <- function(x, vraag, group_vars) {
  x |>
    filter(
      !is.na({{ vraag }}),
      {{ vraag }} != 'niet ingevuld'
    ) |>
    group_by({{ vraag }}, jaar, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    rename(v8 = {{ vraag }})
}


### samenvoegen data 2022

markt_list[["22_bez"]] <- bind_rows(
  markt_list[["22_bez"]][["bez_22_ams"]] |>
    select(v7, jaar, markt, type_markt2, leefklas),
  markt_list[["22_bez"]][["bez_22_wsp"]] |>
    select(v7a, jaar, markt, type_markt2, leefklas) |>
    rename(v7 = v7a)
)


tabel_tevredenheid <- list()

tabel_tevredenheid[["totaal"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_tevr(vraag = v7, group_vars = NULL),
  markt_list[["22_bez"]] |>
    function_tevr(vraag = v7, group_vars = NULL),
  markt_list[["26_bez"]] |>
    function_tevr(vraag = v8, group_vars = NULL)
)

### markt
tabel_tevredenheid[["markt"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_tevr(vraag = v7, group_vars = c("markt")),
  markt_list[["22_bez"]] |>
    function_tevr(vraag = v7, group_vars = c("markt")),
  markt_list[["26_bez"]] |>
    function_tevr(vraag = v8, group_vars = c("markt"))
)

tabel_tevredenheid[["leefklas"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_tevr(vraag = v7, group_vars = c("leefklas")),

  markt_list[["22_bez"]] |>
    function_tevr(vraag = v7, group_vars = c("leefklas")),

  markt_list[["26_bez"]] |>
    function_tevr(vraag = v8, group_vars = c("leefklas"))
) |>
  mutate(leefklas = replace_na(leefklas, "leeftijd onbekend"))


tabel_tevredenheid[["type_markt2"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_tevr(vraag = v7, group_vars = c("type_markt2")),

  markt_list[["22_bez"]] |>
    function_tevr(vraag = v7, group_vars = c("type_markt2")),

  markt_list[["26_bez"]] |>
    function_tevr(vraag = v8, group_vars = c("type_markt2"))
)

write.xlsx(tabel_tevredenheid, "05 output tabellen/tabel_v8_tevredenheid.xlsx")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


tabel_tevredenheid[["totaal"]] |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(jaar),
    fillvar = fct_reorder(v8, aandeel),
    color_pal = os_blauw
  )

ggsave("06 output figuren/fig_v8_tevr_totaal.svg", width = 12, height = 6)


tabel_tevredenheid[['markt']] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(markt),
    fillvar = fct_reorder(v8, aandeel),
    color_pal = os_blauw
  )

ggsave("06 output figuren/fig_v8_tevr_markt.svg", width = 12, height = 6)

bind_rows(
  tabel_tevredenheid[['type_markt2']],
  tabel_tevredenheid[['totaal']] |>
    add_column(type_markt2 = 'totaal')
) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(type_markt2),
    fillvar = fct_reorder(v8, aandeel),
    color_pal = os_blauw
  ) +
  facet_wrap(~jaar)

ggsave("06 output figuren/fig_v8_tevr_markt2.svg", width = 12, height = 6)


bind_rows(
  tabel_tevredenheid[['leefklas']],
  tabel_tevredenheid[['totaal']] |>
    add_column(leefklas = 'totaal')
) |>
  filter(
    leefklas != 'leeftijd onbekend'
  ) |>
  mutate(leefklas = factor(leefklas, levels = levels_leefklas)) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(leefklas),
    fillvar = fct_reorder(v8, aandeel),
    color_pal = os_blauw
  ) +
  facet_wrap(~jaar)

ggsave("06 output figuren/fig_v8_tevr_lft.svg", width = 12, height = 6)
