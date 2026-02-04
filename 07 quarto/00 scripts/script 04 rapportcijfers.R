### frequentie monitor detailhandel

# source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")

os_blauw <- c(
  "#e6e6e6",
  "#dcddee",
  "#b8bcdd",
  "#959dcc",
  "#707ebb",
  "#4861aa",
  "#004699"
)

my_markt_selection <- function(x, markt_selectie) {
  bind_rows(
    # markt
    x |>
      filter(groep == 'bezoekers') |>
      filter(markt == markt_selectie) |>
      filter(leefklas == 'totaal') |>
      filter(locatie == 'totaal'),

    # totaal
    x |>
      filter(
        groep == 'bezoekers',
        markt == 'totaal',
        type_markt2 == 'totaal',
        leefklas == 'totaal',
        locatie == 'totaal'
      ),

    # meerdaags of eendaags
    x |>
      filter(
        groep == 'bezoekers',
        markt == 'totaal',
        leefklas == 'totaal',
        locatie == 'totaal'
      ) |>
      filter(
        if (markt_selectie %in% levels_markt_eendaags) {
          type_markt2 == 'eendaagse markt'
        } else {
          type_markt2 == 'markt op meerdere dagen'
        }
      ) |>
      dplyr::select(-c("markt")) |>
      rename(markt = type_markt2)
  )
}


## stack figuur markt links totaal rechts

my_stack_figure <- function(tabel, vraag, naam) {
  levels_markt |>
    map(\(x) {
      filter(tabel, markt %in% c(x, "totaal")) |>
        fun_totaal(
          xvar = aandeel,
          yvar = fct_rev(jaar),
          fillvar = fct_rev({{ vraag }}),
          color_pal = os_blauw
        ) +

        facet_wrap(~ fct_relevel(markt, "totaal", after = Inf)) +
        guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
    }) |>
    set_names(levels_markt) |>
    write_rds(glue::glue("07 quarto/02 figuren/fig_{ naam }.rds"))
}


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
      add_column(markt = 'totaal'),
    rapportcijfers_ond[["totaal"]] |>
      add_column(markt = 'totaal')
  ),

  bind_rows(
    rapportcijfers_bez[["markt"]],
    rapportcijfers_ond[["markt"]]
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
    geom_text(aes(label = round(gemiddelde, 1)), family = font) +
    labs(title = NULL, x = NULL, y = NULL) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlgn")) +
    theme_os() +
    facet_wrap(~markt)
}


levels_markt |>
  map(\(x) {
    filter(
      rap_cijfers,
      markt %in% c(x, "totaal")
    ) |>
      fig_rap_functie(groep = "bezoekers")
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_rapportcijfers_bez.rds")

levels_markt |>
  map(\(x) {
    filter(
      rap_cijfers,
      markt %in% c(x, "totaal")
    ) |>
      fig_rap_functie(groep = "ondernemers")
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_rapportcijfers_ond.rds")


#### bezettingsgraden ---

bezettingsgraden <- read_rds("03 intermediate/tabel_bezettingsgraden.rds")


levels_markt |>
  map(\(x) {
    filter(
      bezettingsgraden,
      name != 'Eindtotaal',
      markt %in% c(x, "totaal")
    ) |>
      mutate(name = ym(name)) |>
      ggplot(aes(y = value, x = name, color = markt)) +
      geom_line(linewidth = 1) +
      geom_text(aes(label = label_percent(accuracy = 1)(value)), vjust = -1) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        labels = label_percent(),
        expand = c(0, 0)
      ) +
      labs(y = NULL, x = NULL) +
      scale_color_manual(values = c("#004699", "#6cbd74")) +
      theme_os_line(legend_position = 'bottom') +
      guides(labels = 'none')
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_bezettingsgraden.rds")
