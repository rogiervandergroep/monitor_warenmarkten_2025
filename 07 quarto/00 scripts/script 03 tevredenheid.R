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
        guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
    }) |>
    set_names(levels_markt) |>
    write_rds(glue::glue("07 quarto/02 figuren/fig_{ naam }.rds"))
}

### v8 tevredenheid

tabel_v8 <- read_rds("03 intermediate/tabel_v8_tevredenheid.rds")


tabel_v8_def <- bind_rows(
  tabel_v8[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v8[["markt"]]
)

tabel_v8_def |>
  my_stack_figure(vraag = v8, naam = "v8_tevr")


v8_goed <- tabel_v8_def |>
  filter(
    jaar == 'jaar 2025',
    v8 == 'goed'
  ) |>
  write_rds("07 quarto/03 data/v8_goed.rds")


# redenen_ontevreden

tabel_v9 <- read_rds("03 intermediate/tabel_v9_ontevr.rds")


tabel_v9_def <- bind_rows(
  tabel_v9[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v9[["markt"]]
) |>
  filter(
    jaar == 'jaar 2025',
    value == 'Yes'
  ) |>
  mutate(
    labels = case_when(
      labels == 'anders, namelijk' ~ 'anders',
      TRUE ~ labels
    )
  ) |>
  filter(labels != 'weet ik niet')


#
levels_markt |>
  map(\(x) {
    filter(tabel_v9_def, markt %in% c(x, "totaal")) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(fct_reorder(labels, aandeel), "anders"),
        afr = 0
      ) +
      guides(color = 'none', fill = 'none') +
      scale_x_continuous(labels = scales::percent) +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v9_ontevr.rds")


### wat mist u

tabel_v10_mist_bez <- read_rds("03 intermediate/tabel_v10_watmistu.rds") |>
  map(\(x) filter(x, jaar == 'jaar 2025'))

tabel_mist_bez <- bind_rows(
  tabel_v10_mist_bez[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v10_mist_bez[["markt"]]
)

tabel_v10_mist_ond <- read_rds("03 intermediate/tabel_v10_watmistu_ond.rds") |>
  map(\(x) filter(x, jaar == 'jaar 2025'))


tabel_mist_ond <- bind_rows(
  tabel_v10_mist_ond[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v10_mist_ond[["markt"]]
)


# mist bezoekers
levels_markt |>
  map(\(x) {
    filter(tabel_mist_bez, markt %in% c(x, "totaal")) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(
          fct_reorder(labels, aandeel),
          "niets, markt is goed zo",
          "anders"
        ),
        afr = 0
      ) +
      scale_x_continuous(labels = scales::percent) +
      guides(color = 'none', fill = 'none') +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v11_mist_bez.rds")

# mist ondernemers
levels_markt |>
  map(\(x) {
    filter(tabel_mist_ond, markt %in% c(x, "totaal")) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(
          fct_reorder(labels, aandeel),
          "niets, markt is goed zo",
          "anders"
        ),
        afr = 0
      ) +
      scale_x_continuous(labels = scales::percent) +
      guides(color = 'none', fill = 'none') +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v11_mist_ond.rds")


### beter slechter dan andere markten

tabel_v7_beterslecht <- read_rds(
  "03 intermediate/tabel_v7_ond_beterslechter.rds"
)


tabel_v7 <- bind_rows(
  tabel_v7_beterslecht[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v7_beterslecht[["markt"]]
)

tabel_v7 |>
  my_stack_figure(vraag = v7, naam = "v7_beterslechter")

tabel_v12 <- read_rds(
  "03 intermediate/tabel_v12_helpen.rds"
)

tabel_v12_def <- bind_rows(
  tabel_v12[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v12[["markt"]]
) |>
  filter(
    value == 'Yes',
    labels != 'weet niet, geen antwoord'
  ) |>
  mutate(
    labels = case_when(
      labels == "anders, namelijk:" ~ "anders",
      labels ==
        "markt op een andere dag/dagen, namelijk:" ~ "markt op een andere dag/dagen",
      labels ==
        "meer bijzondere belevenissen of speciale evenementen op de markt" ~ "belevenissen, evenementen",
      TRUE ~ labels
    )
  )


levels_markt |>
  map(\(x) {
    filter(tabel_v12_def, markt %in% c(x, "totaal")) |>
      fun_totaal_een(
        xvar = aandeel,
        yvar = fct_relevel(
          fct_reorder(labels, aandeel),
          "niets",
          "weet niet, geen antwoord",
          "anders",
        ),
        afr = 0
      ) +
      scale_x_continuous(labels = scales::percent) +
      guides(color = 'none', fill = 'none') +
      facet_wrap(~ fct_relevel(markt, "totaal", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v12_helpen.rds")


open_antw <- readr::read_rds("03 intermediate/tab_v9_v12_openant.rds")

my_open_function <- function(x, vraag) {
  x |>

    select(markt, {{ vraag }}) |>
    filter({{ vraag }} != '') |>
    group_by(markt) |>
    summarise(vraag_open = paste({{ vraag }}, collapse = "; "))
}

tab <- list()

tab$v9_open <- open_antw |>
  my_open_function(v9_other4)

tab$v10_other14 <- open_antw |>
  my_open_function(v10_other14)

tab$v10_other15 <- open_antw |>
  my_open_function(v10_other15)

tab$v12_other6 <- open_antw |>
  my_open_function(v12_other6)

tab$v12_other1 <- open_antw |>
  my_open_function(v12_other1)

write_rds(tab, "07 quarto/03 data/tab_openant.rds")


### v15 : markt op afstand

tabel_v15_marktopafstand <- read_rds(
  "03 intermediate/tabel_v15_ond_opafstand.rds"
)

tabel_15 <- bind_rows(
  tabel_v15_marktopafstand[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v15_marktopafstand[["markt"]]
)

tabel_15 |>
  my_stack_figure_eenjaar(vraag = v15, naam = "v15_opafstand")

#
#
#  v16a ziet u zich nog over tien jaar?

tabel_v16_tienjaar <- read_rds(
  "03 intermediate/tabel_v16a_ond_tienjaar.rds"
)

tabel_16 <- bind_rows(
  tabel_v16_tienjaar[["totaal"]] |>
    add_column(markt = 'totaal'),
  tabel_v16_tienjaar[["markt"]]
)

tabel_16 |>
  my_stack_figure_eenjaar(vraag = v16a, naam = "v16_tienjaar")

#  v16b toelichting?
