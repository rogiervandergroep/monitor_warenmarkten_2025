source("07 quarto/00 scripts/script 00 plot functies.R")


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

tab_v9_max <- tabel_v9[["markt"]] |>
  filter(
    value == 'Yes',
    jaar == 'jaar 2025'
  ) |>
  group_by(markt) |>
  slice_max(aandeel, n = 1) |>
  write_rds("07 quarto/03 data/tab_v9_reden_ont_max.rds")


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
