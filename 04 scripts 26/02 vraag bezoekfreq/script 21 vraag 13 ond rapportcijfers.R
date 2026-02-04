library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")


# vraag 09  rapportcijfers items
# vraag 13  algemeen rapportcijfer

v11_2016 <- markt_list[["16_ond"]] |>
  select(
    v10_1,
    v10_2,
    v10_3,
    v10_4,
    v10_5,
    v10_6,
    v10_7,
    v10_8,
    v10_9,
    v10_11,
    v13_ond
  ) |>
  names()


v11_2022 <- markt_list[["22_ond"]][["ond_22_ams"]] |>
  select(
    v8_variatie_in_het_rapportcijfer:v8_faciliteiten_rapportcijfer,
    v12
  ) |>
  names()

v11_2026 <- markt_list[["26_ond"]] |>
  select(all_of(starts_with("v9_")), v13) |>
  names()


markt_list[["22_ond_ams_wsp"]] <- bind_rows(
  markt_list[["22_ond"]][["ond_22_ams"]] |>
    select(
      all_of(v11_2022),
      jaar,
      markt,
      type_markt2,
      stadsdeel_markt,
      leefklas
    ),
  markt_list[["22_ond"]][["ond_22_wsp"]] |>
    select(
      all_of(v11_2022),
      jaar,
      markt,
      type_markt2,
      stadsdeel_markt,
      leefklas
    )
)


df_labels_16 <- tibble::tibble(
  name = v11_2016,
  labels = c(
    "variatie in het eetbare productaanbod",
    "variatie in het niet-eetbare productaanbod",
    "uitstraling van de marktkramen",
    "sfeer/gezelligheid op de markt",
    "indeling en opstelling van de markt",
    "bereikbaarheid van de markt",
    "stallingsmogelijkheden voor fiets/scooter",
    "netheid/verzorgdheid",
    "reclame en acties",
    "faciliteiten",
    "algemeen rapportcijfer"
  )
)


df_labels_22 <- tibble::tibble(
  name = v11_2022,
  labels = c(
    "variatie in het eetbare productaanbod",
    "variatie in het niet-eetbare productaanbod",
    "uitstraling van de marktkramen",
    "sfeer/gezelligheid op de markt",
    "indeling en opstelling van de markt",
    "bereikbaarheid van de markt",
    "stallingsmogelijkheden voor fiets/scooter",
    "netheid/verzorgdheid",
    "reclame en acties",
    "faciliteiten",
    "algemeen rapportcijfer"
  )
)


df_labels_26 <- tibble::tibble(
  name = v11_2026,
  labels = c(
    "variatie in het eetbare productaanbod",
    "variatie in het niet-eetbare productaanbod",
    "uitstraling van de marktkramen",
    "sfeer/gezelligheid op de markt",
    "indeling en opstelling van de markt",
    "bereikbaarheid van de markt",
    "stallingsmogelijkheden voor fiets/scooter",
    "netheid/verzorgdheid",
    "reclame en acties",
    "faciliteiten",
    "algemeen rapportcijfer"
  )
)


function_rapportijfer <- function(x, i, group_vars) {
  x |>
    pivot_longer(i) |>
    mutate(value = as.numeric(value)) |>
    group_by(jaar, name, across(all_of(group_vars))) |>
    summarise(
      aantal = n(),
      gemiddelde = mean(value, na.rm = TRUE)
    )
}

df_rapportcijfers <- list()

df_rapportcijfers[['totaal']] <- bind_rows(
  markt_list[["26_ond"]] |>
    function_rapportijfer(
      i = v11_2026,
      group_vars = NULL
    ) |>
    left_join(df_labels_26, by = "name"),

  markt_list[["22_ond_ams_wsp"]] |>
    function_rapportijfer(
      i = v11_2022,
      group_vars = NULL
    ) |>
    left_join(df_labels_22, by = "name"),

  markt_list[["16_ond"]] |>
    function_rapportijfer(
      i = v11_2016,
      group_vars = NULL
    ) |>
    left_join(df_labels_16, by = "name")
)


df_rapportcijfers[['markt']] <- bind_rows(
  markt_list[["26_ond"]] |>
    function_rapportijfer(
      i = v11_2026,
      group_vars = c("markt")
    ) |>
    left_join(df_labels_26, by = "name"),

  markt_list[["22_ond_ams_wsp"]] |>
    function_rapportijfer(
      i = v11_2022,
      group_vars = c("markt")
    ) |>
    left_join(df_labels_22, by = "name"),

  markt_list[["16_ond"]] |>
    function_rapportijfer(
      i = v11_2016,
      group_vars = c("markt")
    ) |>
    left_join(df_labels_16, by = "name")
)


df_rapportcijfers[['stadsdeel_markt']] <- bind_rows(
  markt_list[["26_ond"]] |>
    function_rapportijfer(
      i = v11_2026,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_26, by = "name"),

  markt_list[["22_ond_ams_wsp"]] |>
    function_rapportijfer(
      i = v11_2022,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_22, by = "name"),

  markt_list[["16_ond"]] |>
    function_rapportijfer(
      i = v11_2016,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_16, by = "name")
)


df_rapportcijfers[['stadsdeel_markt']] <- bind_rows(
  markt_list[["26_ond"]] |>
    function_rapportijfer(
      i = v11_2026,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_26, by = "name"),

  markt_list[["22_ond_ams_wsp"]] |>
    function_rapportijfer(
      i = v11_2022,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_22, by = "name"),

  markt_list[["16_ond"]] |>
    function_rapportijfer(
      i = v11_2016,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_16, by = "name")
)


df_rapportcijfers[['type_markt2']] <- bind_rows(
  markt_list[["26_ond"]] |>
    function_rapportijfer(
      i = v11_2026,
      group_vars = c("type_markt2")
    ) |>
    left_join(df_labels_26, by = "name"),

  markt_list[["22_ond_ams_wsp"]] |>
    function_rapportijfer(
      i = v11_2022,
      group_vars = c("type_markt2")
    ) |>
    left_join(df_labels_22, by = "name"),

  markt_list[["16_ond"]] |>
    function_rapportijfer(
      i = v11_2016,
      group_vars = c("type_markt2")
    ) |>
    left_join(df_labels_16, by = "name")
)


df_rapportcijfers[['stadsdeel_markt']] <- bind_rows(
  markt_list[["26_ond"]] |>
    function_rapportijfer(
      i = v11_2026,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_26, by = "name"),

  markt_list[["22_ond_ams_wsp"]] |>
    function_rapportijfer(
      i = v11_2022,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_22, by = "name"),

  markt_list[["16_ond"]] |>
    function_rapportijfer(
      i = v11_2016,
      group_vars = c("stadsdeel_markt")
    ) |>
    left_join(df_labels_16, by = "name")
)


write.xlsx(
  df_rapportcijfers,
  "05 output tabellen/tabel_v11_rapportcijfers_ond.xlsx"
)


write_rds(
  df_rapportcijfers,
  "03 intermediate/tabel_v11_rapportcijfers_ond.rds"
)

source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")


## functie om antwoorden op te schonen
my_filter <- function(x) {
  x |>
    filter(
      markt != 'Hermitagemarkt',
      markt != 'Nieuwmarkt',
      markt != 'Pekmarkt',
      markt != 'Zuidasmarkt',
      markt != 'Westerstraat'
    )
}

## opschonen antwoorden voor figuren
rap_c_ond <- df_rapportcijfers[["markt"]] |>
  my_filter() |>
  add_column(groep = 'ondernemers')


bind_rows(rap_c_bez, rap_c_ond) |>
  filter(labels == 'algemeen rapportcijfer') |>
  mutate(jaar = str_remove_all(jaar, "jaar ")) |>

  ggplot(aes(
    x = jaar,
    y = markt,
    fill = round(gemiddelde, 1)
  )) +
  geom_tile(color = "white", lwd = 0.9, linetype = 1) +
  geom_text(aes(label = round(gemiddelde, 1)), family = font) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os() +
  facet_wrap(~groep)


ggsave(
  "06 output figuren/fig_v11_rap_tile.svg",
  width = 12,
  height = 8
)


rap_c_ond_items <- df_rapportcijfers[["totaal"]] |>
  add_column(groep = "ondernemers")

items <- c(
  "kant en klaar voedsel",
  "indeling en opstelling van de markt",
  "netheid/verzorgdheid",
  "openingstijden",
  "parkeermogelijkheden en tarieven",
  "sfeer/gezelligheid op de markt",
  "variatie in het niet-eetbare productaanbod",
  "variatie in het eetbare productaanbod",
  "algemeen rapportcijfer",

  "reclame en acties"
)


bind_rows(rap_c_bez_items, rap_c_ond_items) |>
  mutate(
    jaar = str_remove_all(jaar, "jaar "),
    labels = str_remove_all(labels, "[\\\\()]"),
    labels = str_replace_all(
      labels,
      "aanbod voedsel voor directe consumptie kant-en-klaar voedsel",
      "kant en klaar voedsel"
    )
  ) |>
  filter(labels %in% items) |>

  ggplot(aes(
    x = jaar,
    y = labels,
    fill = round(gemiddelde, 1)
  )) +
  geom_tile(color = "white", lwd = 0.9, linetype = 1) +
  geom_text(aes(label = round(gemiddelde, 1)), family = font) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
  theme_os() +
  facet_wrap(~groep)


ggsave(
  "06 output figuren/fig_v11_rap_tile_items.svg",
  width = 12,
  height = 6
)


###

df_rapportcijfers[["markt"]] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal_een(
    xvar = gemiddelde,
    afr = 1,
    yvar = fct_reorder(markt, gemiddelde)
  ) +
  facet_wrap(~labels)

ggsave(
  "06 output figuren/fig_v11_rap_ond_totaal.svg",
  width = 12,
  height = 8
)


df_rapportcijfers[["markt"]] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal_een(
    xvar = gemiddelde,
    afr = 1,
    yvar = fct_reorder(markt, gemiddelde)
  ) +
  facet_wrap(~labels)

ggsave(
  "06 output figuren/fig_v11_rap_ond_markt.svg",
  width = 12,
  height = 8
)


df_rapportcijfers[["type_markt2"]] |>
  filter(jaar == 'jaar 2025') |>
  fun_totaal_een(
    xvar = gemiddelde,
    afr = 1,
    yvar = fct_reorder(labels, gemiddelde)
  ) +
  facet_wrap(~type_markt2)

ggsave(
  "06 output figuren/fig_v11_rap_ond_type_markt2.svg",
  width = 12,
  height = 8
)

df_rapportcijfers[["markt"]] |>
  filter(labels == 'algemeen rapportcijfer') |>
  fun_totaal_een(
    xvar = gemiddelde,
    afr = 1,
    yvar = fct_reorder(markt, gemiddelde)
  ) +
  facet_wrap(~jaar)

ggsave(
  "06 output figuren/fig_v11_rap_ond_algemeen.svg",
  width = 12,
  height = 8
)
