# vraag 5 ondernemers
# Hoe is de samenwerking met ...

library(tidyverse)
library(openxlsx)

## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

# 2016: v95_1, v95_2, v95_3, v95_4
# 2022: v5_marktondernemers_gv1, v5_marktondernemers2_gv1, v5_marktondernemers3_gv1, v5_marktondernemers4_gv1,
# 2025: v5_marktondernemers_gv1, v5_marktondernemers2_gv1, v5_marktondernemers3_gv1, v5_marktondernemers4_gv1,

# martkondernemers onderling
# en winkeliers
# horecaondernemers
# stadsdeel

function_vraag <- function(x, vraag, group_vars, omschrijving) {
  x |>
    filter(
      !is.na({{ vraag }}),
      {{ vraag }} != 'niet ingevuld'
    ) |>
    group_by({{ vraag }}, jaar, across(all_of(group_vars))) |>
    summarise(aantal = n()) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal)) |>
    rename(v5 = {{ vraag }}) |>
    add_column(samenwerking_met = omschrijving)
}


### samenvoegen data 2022

markt_list[["22_ond"]] <- bind_rows(
  markt_list[["22_ond"]][["ond_22_ams"]] |>
    select(
      v5_marktondernemers_gv1:v5_marktondernemer4_gv1,
      jaar,
      markt,
      type_markt2,
      leefklas
    ),
  markt_list[["22_ond"]][["ond_22_wsp"]] |>
    select(
      v5_marktondernemers_gv1:v5_marktondernemer4_gv1,
      jaar,
      markt,
      type_markt2,
      leefklas
    )
)


tabel_v5_samenwerking <- list()

tabel_v5_samenwerking[["totaal"]] <- bind_rows(
  # ondernemers onderling
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_1,
      group_vars = NULL,
      omschrijving = "ondernemers"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemers_gv1,
      group_vars = NULL,
      omschrijving = "ondernemers"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemers_gv1,
      group_vars = NULL,
      omschrijving = "ondernemers"
    ),

  # winkeliers
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_2,
      group_vars = NULL,
      omschrijving = "winkeliers"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer2_gv1,
      group_vars = NULL,
      omschrijving = "winkeliers"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer2_gv1,
      group_vars = NULL,
      omschrijving = "winkeliers"
    ),

  # horeca
  markt_list[["16_ond"]] |>
    function_vraag(vraag = v95_3, group_vars = NULL, omschrijving = "horeca"),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer3_gv1,
      group_vars = NULL,
      omschrijving = "horeca"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer3_gv1,
      group_vars = NULL,
      omschrijving = "horeca"
    ),

  # stadsdeel
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_4,
      group_vars = NULL,
      omschrijving = "stadsdeel, gemeente"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer4_gv1,
      group_vars = NULL,
      omschrijving = "stadsdeel, gemeente"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer4_gv1,
      group_vars = NULL,
      omschrijving = "stadsdeel, gemeente"
    )
)


# pewr markt
tabel_v5_samenwerking[["markt"]] <- bind_rows(
  # ondernemers onderling
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_1,
      group_vars = c("markt"),
      omschrijving = "ondernemers"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemers_gv1,
      group_vars = c("markt"),
      omschrijving = "ondernemers"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemers_gv1,
      group_vars = c("markt"),
      omschrijving = "ondernemers"
    ),

  # winkeliers
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_2,
      group_vars = c("markt"),
      omschrijving = "winkeliers"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer2_gv1,
      group_vars = c("markt"),
      omschrijving = "winkeliers"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer2_gv1,
      group_vars = c("markt"),
      omschrijving = "winkeliers"
    ),

  # horeca
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_3,
      group_vars = c("markt"),
      omschrijving = "horeca"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer3_gv1,
      group_vars = c("markt"),
      omschrijving = "horeca"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer3_gv1,
      group_vars = c("markt"),
      omschrijving = "horeca"
    ),

  # stadsdeel
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_4,
      group_vars = c("markt"),
      omschrijving = "stadsdeel, gemeente"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer4_gv1,
      group_vars = c("markt"),
      omschrijving = "stadsdeel, gemeente"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer4_gv1,
      group_vars = c("markt"),
      omschrijving = "stadsdeel, gemeente"
    )
)

### type markt 2 ---
tabel_v5_samenwerking[["type_markt2"]] <- bind_rows(
  # ondernemers onderling
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_1,
      group_vars = c("type_markt2"),
      omschrijving = "ondernemers"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemers_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "ondernemers"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemers_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "ondernemers"
    ),

  # winkeliers
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_2,
      group_vars = c("type_markt2"),
      omschrijving = "winkeliers"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer2_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "winkeliers"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer2_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "winkeliers"
    ),

  # horeca
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_3,
      group_vars = c("type_markt2"),
      omschrijving = "horeca"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer3_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "horeca"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer3_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "horeca"
    ),

  # stadsdeel
  markt_list[["16_ond"]] |>
    function_vraag(
      vraag = v95_4,
      group_vars = c("type_markt2"),
      omschrijving = "stadsdeel, gemeente"
    ),
  markt_list[["22_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer4_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "stadsdeel, gemeente"
    ),
  markt_list[["26_ond"]] |>
    function_vraag(
      vraag = v5_marktondernemer4_gv1,
      group_vars = c("type_markt2"),
      omschrijving = "stadsdeel, gemeente"
    )
)


write.xlsx(
  tabel_v5_samenwerking,
  "05 output tabellen/tabel_v5_ond_samenwerking.xlsx"
)

write_rds(
  tabel_v5_samenwerking,
  "03 intermediate/tabel_v5_ond_samenwerking.rds"
)


source("04 scripts 26/00 scr/script 00 plot functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")

tab_v6_toelichting <- markt_list[["26_ond"]] |>
  select(markt, v6) |>
  filter(v6 != '') |>
  group_by(markt) |>
  summarise(v6_open = paste(v6, collapse = "; "))
write_rds(tab_v6_toelichting, "07 quarto/03 data/tab_v6_ond_toel.rds")


tabel_v5_samenwerking[["totaal"]] |>
  mutate(
    samenwerking_met = factor(
      samenwerking_met,
      levels = c("ondernemers", "winkeliers", "horeca", "stadsdeel, gemeente")
    )
  ) |>
  fun_totaal(
    xvar = aandeel * 100,
    yvar = fct_rev(jaar),
    fillvar = fct_rev(v5),
    color_pal = os_blauw[c(1, 3, 4, 6, 7)]
  ) +
  facet_wrap(~samenwerking_met, nrow = 1)

ggsave("06 output figuren/fig_v5_ond_totaal.svg", width = 12, height = 5)

# toevoegen open atnwoorden
