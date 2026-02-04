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

#### vraag respons ---

tabel_respons <- read_rds("03 intermediate/markten_respons.rds")

tabel_respons_totaal <- tabel_respons[['totaal']] |>
  dplyr::select(-c(passanten)) |>
  pivot_longer(cols = c(bezoekers, ondernemers))

tabel_respons_totaal |>
  ungroup() |>
  filter(jaar == 'jaar 2025') |>
  select(markt, name, value) |>
  write_rds("07 quarto/03 data/tab_respons.rds")


levels_markt |>
  map(\(x) {
    filter(tabel_respons_totaal, markt == x) |>
      fun_totaal_een(
        xvar = value,
        yvar = fct_rev(jaar),
        afr = 0
      ) +
      facet_wrap(
        ~name,
        nrow = 1
      ) +
      guides(color = 'none', fill = 'none')
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v0_respons.rds")

# toevoegen lft en herkomst

tab_lft <- tabel_respons[["leeftijd"]] |>
  filter(
    groep != 'passanten',
    jaar == 'jaar 2025'
  )

tab_lft_bez <- tab_lft |>
  filter(groep == 'bezoekers')


tab_lft_ond <- tab_lft |>
  filter(groep == 'ondernemers')


# figuur leeftijd bez
levels_markt |>
  map(\(x) {
    filter(tab_lft_bez, markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(leefklas),
        color_pal = os_blauw[c(1, 3:7)]
      ) +
      labs(title = "leeftijd") +
      theme_os(legend_position = "bottom") +
      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_leeftijd_bez.rds"))


# figuur leeftijd ond
levels_markt |>
  map(\(x) {
    filter(tab_lft_ond, markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(leefklas),
        color_pal = os_blauw[c(1, 3:7)]
      ) +
      labs(title = "leeftijd") +
      theme_os(legend_position = "bottom") +
      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_leeftijd_ond.rds"))

### herkomst bezoekers
tab_herk <- tabel_respons[["locatie"]] |>
  filter(
    groep == 'bezoekers',
    jaar == 'jaar 2025'
  ) |>
  mutate(locatie = factor(locatie, levels = levels_loc_lang))

# figuur herkomst
levels_markt |>
  map(\(x) {
    filter(tab_herk, markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(locatie),
        color_pal = os_blauw[c(2, 4, 7)]
      ) +
      labs(title = "herkomst") +
      guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_herkomst.rds"))


# figuur hoe lang staat u op de markt
tab_ond_lengte <- tabel_respons[["ond_lengte"]] |>
  filter(jaar == 'jaar 2025')

levels_markt |>
  map(\(x) {
    filter(tab_ond_lengte, markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(v1),
        color_pal = os_blauw[c(1, 3:7)]
      ) +
      labs(title = "lengte op de markt")
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_lengte_ond.rds"))

#vaste plek sol
tab_ond_plek <- tabel_respons[["ond_plek"]] |>
  filter(jaar == 'jaar 2025')

levels_markt |>
  map(\(x) {
    filter(tab_ond_plek, markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(v2),
        color_pal = os_blauw[c(1, 5, 7)]
      ) +
      labs(title = "vergunninghouder of sollicitant") +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v0_vast.rds")

# food non food niet als figuur alleen in tekst opnemen
# tab_food <- tabel_respons[["ond_nonfood"]] |>
#   filter(jaar == 'jaar 2025') |>
#   group_by(type_markt2, markt) |>
#   mutate(aandeel = aantal / sum(aantal))
