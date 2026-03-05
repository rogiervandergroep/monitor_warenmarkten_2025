# 9. Waarom bent u ontevreden over het productaanbod?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

v10_2016 <- markt_list[["16_ond"]] |>
  select(v901:v915) |>
  names()

v10_2022_ams <- markt_list[["22_ond"]][["ond_22_ams"]] |>
  select(v901:v915) |>
  names()

v10_2022_wsp <- markt_list[["22_ond"]][["ond_22_wsp"]] |>
  select(v901:v915) |>
  names()

v10_2026 <- markt_list[["26_ond"]] |>
  select(v1001:v1015) |>
  names()


#
markt_list[["22_ond_ams_wsp"]] <- bind_rows(
  markt_list[["22_ond"]][["ond_22_ams"]] |>
    select(all_of(v10_2022_ams), jaar, markt, type_markt2, leefklas),
  markt_list[["22_ond"]][["ond_22_wsp"]] |>
    select(all_of(v10_2022_wsp), jaar, markt, type_markt2, leefklas) |>
    set_names(c(v10_2022_ams, "jaar", "markt", "type_markt2", "leefklas"))
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

v10_watmistu_ond <- list()

v10_watmistu_ond[["totaal"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_vraag(
      i = v10_2016,
      group_vars = NULL
    ),

  markt_list[["22_ond_ams_wsp"]] |>
    function_vraag(
      y = markt_list[["22_ond"]][["ond_22_ams"]],
      i = v10_2022_ams,
      group_vars = NULL
    ),

  markt_list[["26_ond"]] |>
    function_vraag(
      i = v10_2026,
      group_vars = NULL
    )
)


v10_watmistu_ond[["markt"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_vraag(
      i = v10_2016,
      group_vars = c("markt")
    ),

  markt_list[["22_ond_ams_wsp"]] |>
    function_vraag(
      y = markt_list[["22_ond"]][["ond_22_ams"]],
      i = v10_2022_ams,
      group_vars = c("markt")
    ),

  markt_list[["26_ond"]] |>
    function_vraag(
      i = v10_2026,
      group_vars = c("markt")
    )
)


v10_watmistu_ond[["leefklas"]] <- bind_rows(
  markt_list[["16_ond"]] |>
    function_vraag(
      i = v10_2016,
      group_vars = c("leefklas")
    ),

  markt_list[["22_ond_ams_wsp"]] |>
    function_vraag(
      y = markt_list[["22_ond"]][["ond_22_ams"]],
      i = v10_2022_ams,
      group_vars = c("leefklas")
    ),

  markt_list[["26_ond"]] |>
    function_vraag(
      i = v10_2026,
      group_vars = c("leefklas")
    )
)

## functie om antwoorden op te schonen
my_filter <- function(x) {
  x |>

    filter(
      value == 'Yes',
      !is.na(labels),
      labels != 'weet niet, geen antwoord',
      labels != 'niet ingevuld',
      labels != 'weet ik niet'
    ) |>
    mutate(
      labels = str_replace_all(
        labels,
        "biologische- en streekproducten",
        "biologische en streekproducten"
      ),
      labels = str_remove_all(labels, ", namelijk"),
      labels = str_replace_all(labels, "anders:", "anders"),
      labels = str_replace_all(labels, "groenten / fruit", "groenten en fruit"),
      labels = str_replace_all(labels, "groenten / fruit", "groenten en fruit"),
      labels = str_remove_all(labels, "[\\\\?<>:()]"),
      labels = str_replace_all(
        labels,
        "sieraden (juwelen, horloges)",
        "sieraden"
      ),
      labels = str_replace_all(
        labels,
        "media (zoals boeken, muziek of dvd’s)",
        "media"
      ),
      labels = str_replace_all(labels, "anders, namelijk", "anders"),
      labels = str_replace_all(labels, "anders namelijk", "anders")
    )
}

## opschonen antwoorden voor figuren
v10_watmistu_ond <- v10_watmistu_ond |>
  map(\(x) my_filter(x))


write.xlsx(v10_watmistu_ond, "05 output tabellen/tabel_v10_watmistu_ond.xlsx")
write_rds(v10_watmistu_ond, "03 intermediate/tabel_v10_watmistu_ond.rds")


openvraag_10_mist <- markt_list[["26_ond"]] |>
  select(markt, v10_other12, v10_other13)

readr::write_rds(
  openvraag_10_mist,
  "07 quarto/03 data/tab_v10_ond_mist_anders.rds"
)


source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


bind_rows(
  v10_watmistu_bez[['totaal']] |>
    add_column(groep = 'bezoekers') |>
    filter(jaar == 'jaar 2025'),

  v10_watmistu_ond[['totaal']] |>
    add_column(groep = 'ondernemers') |>
    filter(jaar == 'jaar 2025')
) |>

  fun_totaal_een(
    xvar = aandeel * 100,
    yvar = fct_relevel(
      fct_reorder(labels, aandeel),
      "niets, markt is goed zo",
      "anders"
    )
  ) +
  facet_wrap(~groep)


ggsave(
  "06 output figuren/fig_v10_mistopmarkt_totaal.svg",
  width = 12,
  height = 6
)
