source("07 quarto/00 scripts/script 00 plot functies.R")

#### rapportcijfers ----

rapportcijfers_bez <- read_rds(
  "03 intermediate/tabel_v11_rapportcijfers.rds"
) |>
  map(\(x) add_column(x, doelgroep = 'bezoekers'))

rapportcijfers_ond <- read_rds(
  "03 intermediate/tabel_v11_rapportcijfers_ond.rds"
) |>
  map(\(x) add_column(x, doelgroep = 'ondernemers'))

rap_cijfers <- bind_rows(
  bind_rows(
    rapportcijfers_bez[["totaal"]] |>
      add_column(markt = 'alle markten'),
    rapportcijfers_ond[["totaal"]] |>
      add_column(markt = 'alle markten')
  ),

  bind_rows(
    rapportcijfers_bez[["markt"]] |>
      my_markt_rename(),
    rapportcijfers_ond[["markt"]] |>
      my_markt_rename()
  )
)


### tvb antwoorden in code chuncks

rap_alg <- rap_cijfers |>
  filter(
    jaar == "jaar 2025",
    labels == 'algemeen rapportcijfer',
    doelgroep %in% c("ondernemers", "bezoekers")
  ) |>
  ungroup() |>
  select(gemiddelde, doelgroep, markt) |>
  write_rds("07 quarto/03 data/tab_rapportcijfers.rds")


fig_rap_functie <- function(x, groep) {
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

  hcl <- farver::decode_colour(hcl.colors(20, "RdYlgn"), "rgb", "hcl")
  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

  x |>
    filter(doelgroep == groep) |>
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
    geom_text(
      aes(
        color = gemiddelde,
        label = round(gemiddelde, 1)
      ),
      size = 5,
      family = font
    ) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_gradientn(
      colors = hcl.colors(20, "RdYlgn"),
      limits = c(1, 10),
      breaks = c(1, 3, 5.5, 8, 10),
      labels = c(1, 3, 5.5, 8, 10)
    ) +
    scale_color_gradientn(name = NULL, colors = label_col, limits = c(1, 10)) +
    theme_os() +
    theme(text = element_text(size = 15)) +
    facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf)) +
    guides(color = 'none')
}

# rap_cijfers_test <- rap_cijfers |>
#   mutate(gem_cat = gtools::quantcut(gemiddelde))

levels_markt |>
  map(\(x) {
    filter(
      rap_cijfers,
      markt %in% c(x, "alle markten")
    ) |>
      fig_rap_functie(groep = "bezoekers")
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_rapportcijfers_bez.rds")

levels_markt |>
  map(\(x) {
    filter(
      rap_cijfers,
      markt %in% c(x, "alle markten")
    ) |>
      fig_rap_functie(groep = "ondernemers")
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_rapportcijfers_ond.rds")
