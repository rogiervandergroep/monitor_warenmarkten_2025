# 9. Waarom bent u ontevreden over het productaanbod?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

v12_2026 <- markt_list[["26_bez"]] |>
  select(v121:v129) |>
  names()


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

v12_helpen <- list()

v12_helpen[['totaal']] <- markt_list[["26_bez"]] |>
  function_vraag(
    i = v12_2026,
    group_vars = NULL
  )

v12_helpen[['markt']] <- markt_list[["26_bez"]] |>
  function_vraag(
    i = v12_2026,
    group_vars = c("markt")
  )

v12_helpen[['leefklas']] <- markt_list[["26_bez"]] |>
  function_vraag(
    i = v12_2026,
    group_vars = c("leefklas")
  )

v12_helpen[['type_markt2']] <- markt_list[["26_bez"]] |>
  function_vraag(
    i = v12_2026,
    group_vars = c("type_markt2")
  )

# bezigheid
v12_helpen[['v16']] <- markt_list[["26_bez"]] |>
  function_vraag(
    i = v12_2026,
    group_vars = c("v16")
  )


v12_helpen[['v17']] <- markt_list[["26_bez"]] |>
  function_vraag(
    i = v12_2026,
    group_vars = c("v17")
  )
write.xlsx(v12_helpen, "05 output tabellen/tabel_v12_helpen.xlsx")

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


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
        "meer bijzondere belevenissen of speciale evenementen op de markt",
        "meer belevenissen of evenementen"
      ),
      labels = str_remove_all(labels, ", namelijk:")
    )
}


## opschonen antwoorden voor figuren
v12_helpen <- v12_helpen |>
  map(\(x) my_filter(x))


v12_helpen[["totaal"]] |>

  fun_totaal_een(
    xvar = aandeel * 100,
    yvar = fct_relevel(
      fct_reorder(labels, aandeel),
      "niets",
      "anders"
    )
  )


bind_rows(
  v12_helpen[["markt"]],
  v12_helpen[["totaal"]] |>
    add_column(markt = 'totaal')
) |>

  fun_totaal_een(
    xvar = aandeel * 100,
    yvar = fct_relevel(
      fct_reorder(labels, aandeel),
      "niets",
      "anders"
    )
  ) +
  facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))

ggsave(
  "06 output figuren/fig_v12_markt.svg",
  width = 14,
  height = 9
)
