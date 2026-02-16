# script met functies voor figeren

###
library(tidyverse)
# library(sf)
# library(ggspatial)
# library(raster)
# library(prettymapr)
# library(svglite)

### frequentie monitor detailhandel

# source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 levels.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")


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


my_stack_figure_eenjaar <- function(tabel, vraag, naam) {
  levels_markt |>
    map(\(x) {
      filter(tabel, markt %in% c(x, "totaal")) |>
        fun_totaal(
          xvar = aandeel,
          yvar = fct_relevel(markt, 'totaal'),
          fillvar = fct_rev({{ vraag }}),
          color_pal = os_blauw
        ) +
        guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T))
    }) |>
    set_names(levels_markt) |>
    write_rds(glue::glue("07 quarto/02 figuren/fig_{ naam }.rds"))
}
