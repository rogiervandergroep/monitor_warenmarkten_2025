source("07 quarto/00 scripts/script 00 plot functies.R")

#### vraag respons ---

tabel_respons <- read_rds("03 intermediate/markten_respons.rds")

tabel_respons_totaal <- tabel_respons[['totaal']] |>
  dplyr::select(-c(passanten)) |>
  pivot_longer(cols = c(bezoekers, ondernemers))

tabel_respons_totaal |>
  ungroup() |>
  filter(jaar == 'jaar 2025') |>
  dplyr::select(markt, name, value) |>
  write_rds("07 quarto/03 data/tab_respons.rds")


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
    filter(tab_lft, markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(leefklas),
        color_pal = os_blauw[c(1, 3:7)]
      ) +
      theme_os(legend_position = "bottom") +
      facet_wrap(~groep) +
      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_leeftijd.rds"))


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

### thuissituatie

levels_markt |>
  map(\(x) {
    filter(tabel_respons[["arbeidsmarkt"]], markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(v16),
        color_pal = os_blauw
      ) +
      labs(title = "herkomst") +
      guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T))
  }) |>
  set_names(levels_markt)


levels_markt |>
  map(\(x) {
    filter(tabel_respons[["huishoudsituatie"]], markt %in% c(x, "totaal")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "totaal"),
        fillvar = fct_rev(thuissituatie),
        color_pal = os_blauw
      ) +
      labs(title = "herkomst") +
      guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T))
  }) |>
  set_names(levels_markt)


#### ONDERNEMERS ---

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
        color_pal = os_blauw[c(2, 4:7)]
      ) +
      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T)) +
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
      guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v0_vast.rds")

# food non food niet als figuur alleen in tekst opnemen
# tab_food <- tabel_respons[["ond_nonfood"]] |>
#   filter(jaar == 'jaar 2025') |>
#   group_by(type_markt2, markt) |>
#   mutate(aandeel = aantal / sum(aantal))
