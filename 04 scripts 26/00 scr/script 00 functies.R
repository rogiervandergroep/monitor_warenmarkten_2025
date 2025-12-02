library(tidyverse)
library(scales)
library(patchwork)


# check kleurenschema: https://os-amsterdam.gitlab.io/datavisualisatie-onderzoek-en-statistiek/

source(
  'http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R'
)


## een functie om kolommen te selecteren
my_selection <-
  function(x, groupvars) {
    x |>
      select(
        any_of(c(
          "jaar",
          "markt",
          "type_markt",
          "groep",
          "leefklas",
          "locatie"
        )),
        any_of(groupvars)
      )
  }

# de functie my_selection wordt gebruikt om kolommen te selecteren voor een group_bind
my_bind_rows <- function(groupvars) {
  names_16_26 <- c("16_bez", "16_pas", "16_ond", "26_bez", "26_ond")

  bind_rows(
    # data 2016 en 2026
    names_16_26 |>
      map_df(\(x) my_selection(markt_list[[x]], groupvars = groupvars)),

    # data 2022
    markt_list[["22_bez"]][["bez_22_ams"]] |>
      my_selection(groupvars = groupvars),
    markt_list[["22_bez"]][["bez_22_wsp"]] |>
      my_selection(groupvars = groupvars),

    markt_list[["22_pas"]][["pas_22_ams"]] |>
      my_selection(groupvars = groupvars),
    markt_list[["22_pas"]][["pas_22_wsp"]] |>
      my_selection(groupvars = groupvars),

    markt_list[["22_ond"]][["ond_22_ams"]] |>
      my_selection(groupvars = groupvars),
    markt_list[["22_ond"]][["ond_22_wsp"]] |>
      my_selection(groupvars = groupvars)
  )
}
