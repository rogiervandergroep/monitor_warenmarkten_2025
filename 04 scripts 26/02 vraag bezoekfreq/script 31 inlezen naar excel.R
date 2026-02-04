library(tidyverse)

### tabellenrapportage ---

library(purrr)

files <- list.files("03 intermediate", pattern = "\\.rds$", full.names = TRUE)

rds_list <- files |>
  set_names(~ tools::file_path_sans_ext(basename(.x))) |>
  map(readRDS)


excel_list <- list(
  resp_bez = bind_rows(
    rds_list[["markten_respons"]][["totaal"]] |>
      pivot_longer(
        cols = c(bezoekers:passanten),
        names_to = "groep",
        values_to = "aantal"
      ),
    rds_list[["markten_respons"]][["stadsdeel"]],
    rds_list[["markten_respons"]][["leeftijd"]],
    rds_list[["markten_respons"]][["locatie"]]
  ),

  resp_ond = bind_rows(
    rds_list[["markten_respons"]][["ond_lengte"]] |>
      rename(antwoord = v1) |>
      add_column(naam = "hoe lang staat u op deze markt?"),

    rds_list[["markten_respons"]][["ond_plek"]] |>
      rename(antwoord = v2) |>
      add_column(naam = "Heeft u een vaste plek?"),

    rds_list[["markten_respons"]][["ond_verkoop"]] |>
      rename(antwoord = v3) |>
      add_column(naam = "Wat verkoopt u?"),
  ),

  v01_freq = rds_list[["markten_v1_freq"]] |>
    add_column(naam = "hoe vaak bezoekt u deze markt?"),

  v02_reden = rds_list[["markten_v3_redenbezoek_alles"]] |>
    add_column(naam = "wat is reden van uw bezoek?"),

  v03_doel = rds_list[["tabel_v5_voornaamstedoel"]] |>
    add_column(naam = "wat is het voornaamste doel?"),

  v04_verv = rds_list[["tabel_v6_vervoermiddel"]] |>
    add_column(naam = "met welk vervoermiddel bent u naar de markt gekomen?"),

  v05_ander = rds_list[["tabel_v14_anderemarkt"]] |>
    add_column(naam = "Gaat u wel eens naar een andere markt?"),

  v06_tevr = bind_rows(
    rds_list[["tabel_v8_tevredenheid"]][["totaal"]],
    rds_list[["tabel_v8_tevredenheid"]][["markt"]],
    rds_list[["tabel_v8_tevredenheid"]][["leefklas"]]
  ) |>
    add_column(naam = "bent u tevreden over het aanbod op de markt?"),

  v07_ontevr = bind_rows(
    rds_list[["tabel_v9_ontevr"]][["totaal"]],
    rds_list[["tabel_v9_ontevr"]][["markt"]],
    rds_list[["tabel_v9_ontevr"]][["leefklas"]]
  ) |>
    add_column(naam = "waarom bent u ontevreden?"),

  v08_mist = bind_rows(
    bind_rows(
      rds_list[["tabel_v10_watmistu"]][["totaal"]],
      rds_list[["tabel_v10_watmistu"]][["markt"]],
      rds_list[["tabel_v10_watmistu"]][["leefklas"]]
    ) |>
      add_column(groep = 'bezoekers'),

    bind_rows(
      rds_list[["tabel_v10_watmistu_ond"]][["totaal"]],
      rds_list[["tabel_v10_watmistu_ond"]][["markt"]],
      rds_list[["tabel_v10_watmistu_ond"]][["leefklas"]]
    ) |>
      add_column(groep = 'ondernemers')
  ) |>
    add_column(naam = "wat mist u op de markt?"),

  v09_help = bind_rows(
    rds_list[["tabel_v12_helpen"]][["totaal"]],
    rds_list[["tabel_v12_helpen"]][["markt"]]
  ) |>
    add_column(naam = "wat zou helpen oom vaker naar de markt te gaan?"),

  v10_rap = bind_rows(
    bind_rows(
      rds_list[["tabel_v11_rapportcijfers"]][["totaal"]],
      rds_list[["tabel_v11_rapportcijfers"]][["markt"]],
      rds_list[["tabel_v11_rapportcijfers"]][["type_markt2"]],
      rds_list[["tabel_v11_rapportcijfers"]][["stadsdeel_markt"]]
    ) |>
      add_column(groep = 'bezoekers'),

    bind_rows(
      rds_list[["tabel_v11_rapportcijfers_ond"]][["totaal"]],
      rds_list[["tabel_v11_rapportcijfers_ond"]][["markt"]],
      rds_list[["tabel_v11_rapportcijfers_ond"]][["type_markt2"]],
      rds_list[["tabel_v11_rapportcijfers_ond"]][["stadsdeel_markt"]]
    ) |>
      add_column(groep = 'ondernemers')
  ),

  v11_on_af = bind_rows(
    rds_list[["tabel_v15_ond_opafstand"]][["totaal"]],
    rds_list[["tabel_v15_ond_opafstand"]][["markt"]],
    rds_list[["tabel_v15_ond_opafstand"]][["leefklas"]]
  ) |>
    add_column(naam = "Markt op afstand: goed idee?"),

  v12_on_tien = bind_rows(
    rds_list[["tabel_v16a_ond_tienjaar"]][["totaal"]],
    rds_list[["tabel_v16a_ond_tienjaar"]][["markt"]],
    rds_list[["tabel_v16a_ond_tienjaar"]][["leefklas"]]
  ) |>
    add_column(naam = "Staat u op deze markt nog over tien jaar?"),

  v13_on_voor = bind_rows(
    rds_list[["tabel_v4a_ond_voorachter"]][["totaal"]],
    rds_list[["tabel_v4a_ond_voorachter"]][["markt"]],
    rds_list[["tabel_v4a_ond_voorachter"]][["leefklas"]]
  ) |>
    add_column(naam = "Is het aantal bezoekers toe of afgeomen?"),

  v14_on_samenw = bind_rows(
    rds_list[["tabel_v5_ond_samenwerking"]][["totaal"]],
    rds_list[["tabel_v5_ond_samenwerking"]][["markt"]]
  ) |>
    add_column(naam = "Hoe is de samenwerking met?"),

  v15_on_beter = bind_rows(
    rds_list[["tabel_v7_ond_beterslechter"]][["totaal"]],
    rds_list[["tabel_v7_ond_beterslechter"]][["markt"]],
    rds_list[["tabel_v7_ond_beterslechter"]][["leefklas"]]
  ) |>
    add_column(
      naam = "staat de markt er beter of slechter voor dan andere markten?"
    )
)


openxlsx::write.xlsx(
  excel_list,
  "05 output tabellen/tabel_warenmarkten_totaal.xlsx"
)
