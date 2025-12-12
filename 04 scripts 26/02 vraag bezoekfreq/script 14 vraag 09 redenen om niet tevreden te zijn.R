# 9. Waarom bent u ontevreden over het productaanbod?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

redenen_2016 <- c("v81", "v82", "v83", "v84", "v85", "v86")
redenen_2022_ams <- c("v81", "v82", "v83", "v84", "v85", "v86")
redenen_2022_wsp <- c("v8a1", "v8a2", "v8a3", "v8a4", "v8a5", "v8a6")
redenen_2026 <- c("v91", "v92", "v93", "v94", "v95", "v96")

markt_list[["22_bez"]] <- bind_rows(
  markt_list[["22_bez"]][["bez_22_ams"]] |>
    select(all_of(redenen_2022_ams), jaar, markt, type_markt2, leefklas),
  markt_list[["22_bez"]][["bez_22_wsp"]] |>
    select(all_of(redenen_2022_wsp), jaar, markt, type_markt2, leefklas) |>
    set_names(c(redenen_2022_ams, "jaar", "markt", "type_markt2", "leefklas"))
)


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

redenen_ontevreden <- list()


redenen_ontevreden[["totaal"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_vraag(
      i = redenen_2016,
      group_vars = NULL
    ),

  markt_list[["22_bez"]] |>
    function_vraag(
      y = markt_list[["16_bez"]],
      i = redenen_2022_ams,
      group_vars = NULL
    ),

  markt_list[["26_bez"]] |>
    function_vraag(
      i = redenen_2026,
      group_vars = NULL
    )
)


redenen_ontevreden[["markt"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_vraag(
      i = redenen_2016,
      group_vars = c("markt")
    ),

  markt_list[["22_bez"]] |>
    function_vraag(
      y = markt_list[["16_bez"]],
      i = redenen_2022_ams,
      group_vars = c("markt")
    ),

  markt_list[["26_bez"]] |>
    function_vraag(
      i = redenen_2026,
      group_vars = c("markt")
    )
)


redenen_ontevreden[["leefklas"]] <- bind_rows(
  markt_list[["16_bez"]] |>
    function_vraag(
      i = redenen_2016,
      group_vars = c("leefklas")
    ),

  markt_list[["22_bez"]] |>
    function_vraag(
      y = markt_list[["16_bez"]],
      i = redenen_2022_ams,
      group_vars = c("leefklas")
    ),

  markt_list[["26_bez"]] |>
    function_vraag(
      i = redenen_2026,
      group_vars = c("leefklas")
    )
)


write.xlsx(redenen_ontevreden, "05 output tabellen/tabel_v9_ontevred.xlsx")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


## functie om antwoorden op te schonen
my_filter <- function(x) {
  x |>

    filter(
      value == 'Yes',
      !is.na(labels),
      labels != 'weet ik niet'
    ) |>
    mutate(
      labels = str_replace_all(labels, "anders, namelijk", "anders")
    )
}

## opschonen antwoorden voor figuren
redenen_ontevreden <- redenen_ontevreden |>
  map(\(x) my_filter(x))


redenen_ontevreden[["totaal"]] |>

  fun_totaal_een(
    xvar = aandeel * 100,
    yvar = fct_relevel(fct_reorder(labels, aandeel), "anders")
  ) +
  facet_wrap(~jaar)

ggsave("06 output figuren/fig_v9_ontevr_totaal.svg", width = 12, height = 4)


redenen_ontevreden[['markt']] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal_een(
    xvar = aantal,
    yvar = fct_relevel(fct_reorder(labels, aandeel), "anders")
  ) +
  facet_wrap(~markt)

ggsave("06 output figuren/fig_v9_ontevr_markt.svg", width = 12, height = 6)
