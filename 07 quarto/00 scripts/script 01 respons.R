source("07 quarto/00 scripts/script 00 plot functies.R")

#### vraag respons ---

onderdeel <- c(
  "totaal",
  "leeftijd",
  "locatie",
  "huishoudsituatie",
  "arbeidsmarkt",
  "ond_lengte",
  "ond_plek",
  "ond_verkoop",
  "ond_food",
  "ond_nonfood"
)

tabel_respons <- read_rds("03 intermediate/markten_respons.rds")


tabel_respons <- onderdeel |>
  map(\(x) {
    mutate(
      tabel_respons[[x]],
      markt = case_when(
        markt == "totaal" ~ "alle markten",
        markt == "Plein 40-45" ~ "Plein '40-'45",
        markt == "Tussenmeer" ~ "Tussen Meer",
        TRUE ~ markt
      )
    )
  }) |>
  set_names(onderdeel)

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
    groep != 'passanten'
  )


tab_lft_bez <- tab_lft |>
  filter(groep == 'bezoekers')


tab_lft_ond <- tab_lft |>
  filter(groep == 'ondernemers')


# figuur leeftijd bez
levels_markt |>
  map(\(x) {
    filter(tab_lft_bez, markt %in% c(x, "alle markten")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(leefklas),
        color_pal = os_blauw[c(1, 3, 5, 7, 9)]
      ) +
      theme_os(legend_position = "bottom") +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf)) +
      guides(color = 'none', fill = guide_legend(ncol = 3, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_leeftijd_bez.rds"))


# figuur leeftijd ond
levels_markt |>
  map(\(x) {
    filter(tab_lft_ond, markt %in% c(x, "alle markten")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(leefklas),
        color_pal = os_blauw[c(1, 3, 5, 7, 9)]
      ) +
      theme_os(legend_position = "bottom") +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf)) +
      guides(color = 'none', fill = guide_legend(ncol = 3, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_leeftijd_ond.rds"))

###

aandeel_bez_lft <- tab_lft_bez |>
  ungroup() |>
  filter(
    jaar == 'jaar 2025',
    leefklas == 'jonger dan 35 jaar'
  ) |>
  select(markt, aandeel) |>
  write_rds("07 quarto/03 data/tab_bez_lft35.rds")


aandeel_ond_lft <- tab_lft_ond |>
  ungroup() |>
  filter(
    jaar == 'jaar 2025',
    leefklas %in% c("tussen 56 en 67 jaar", "68 jaar en ouder")
  ) |>
  group_by(markt) |>
  summarise(aandeel = sum(aandeel)) |>
  write_rds("07 quarto/03 data/tab_ond_lft56.rds")


### herkomst bezoekers naar stadsdeel
tab_herk <- tabel_respons[["locatie"]] |>
  filter(
    groep == 'bezoekers'
  ) |>
  mutate(locatie = factor(locatie, levels = levels_loc_lang))

### herkomst bezoekers naar wijk
tab_wijk <- tabel_respons[["wijk"]] |>
  filter(
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  group_by(markt) |>
  filter(
    !gebied_wijk_naam %in%
      c("overig Nederland", "buitenland", "woonplaats onbekend")
  ) |>
  slice_max(aantal, n = 5)


# figuur herkomst
levels_markt |>
  map(\(x) {
    filter(tab_herk, markt %in% c(x, "alle markten")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(locatie),
        color_pal = discreet[c(10, 5, 7, 3, 1)]
      ) +

      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf)) +
      guides(color = 'none', fill = guide_legend(ncol = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_herkomst_bez.rds")

#

levels_markt |>
  map(\(x) {
    filter(tab_wijk, markt == x) |>
      fun_totaal_een(
        xvar = aantal,
        yvar = fct_relevel(
          fct_reorder(gebied_wijk_naam, aantal),
          "woonplaats onbekend",
          "buitenland",
          "overig Nederland"
        ),
        verm_factor = 1,
        afr = 0
      )
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_wijk_bez.rds")

#

tab_herk_eigen <- tabel_respons[["locatie"]] |>
  filter(
    locatie == 'woont in zelfde stadsdeel markt',
    jaar == 'jaar 2025',
    groep == 'bezoekers'
  ) |>
  ungroup() |>
  select(markt, aandeel) |>
  write_rds("07 quarto/03 data/tab_bez_herk.rds")


### thuissituatie

tabel_arbeidsmarkt <- tabel_respons[["arbeidsmarkt"]] |>
  mutate(
    v16 = case_when(
      v16 == 'anders, namelijk:' ~ 'anders',
      v16 == 'werk (parttime/fulltime/ondernemer/ZZP)' ~ 'werk',
      is.na(v16) ~ 'onbekend',
      TRUE ~ v16
    )
  ) |>
  mutate(
    v16 = factor(
      v16,
      levels = c(
        "werk",
        "student/scholier",
        "gepensioneerd",
        "werkloos",
        "anders",
        "onbekend"
      )
    )
  )


# een jaar alleen 2025
levels_markt |>
  map(\(x) {
    filter(tabel_arbeidsmarkt, markt %in% c(x, "alle markten")) |>
      fun_totaal(
        fillvar = fct_rev(v16),
        xvar = aandeel,
        yvar = fct_relevel(markt, "alle markten"),
        color_pal = discreet[c(10, 3, 2, 5, 7, 9)]
      ) +
      labs(title = "arbeidsmarkt") +
      guides(color = 'none', fill = guide_legend(ncol = 3, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_arbmarkt_bez.rds"))


levels_markt |>
  map(\(x) {
    filter(
      tabel_respons[["huishoudsituatie"]],
      markt %in% c(x, "alle markten")
    ) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, "alle markten"),
        fillvar = fct_relevel(fct_rev(thuissituatie), "anders of onbekend"),
        color_pal = discreet[c(10, 3, 2, 5, 7, 9)]
      ) +
      labs(title = "herkomst") +
      guides(color = 'none', fill = guide_legend(ncol = 3, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_huishoudsit_bez.rds"))


#### ONDERNEMERS ---

# figuur hoe lang staat u op de markt
tab_ond_lengte <- tabel_respons[["ond_lengte"]]

levels_markt |>
  map(\(x) {
    filter(tab_ond_lengte, markt %in% c(x, "alle markten")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(v1),
        color_pal = os_blauw[c(1, 2, 4, 6, 8, 9)]
      ) +
      guides(color = 'none', fill = guide_legend(ncol = 3, reverse = T)) +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds(glue::glue("07 quarto/02 figuren/fig_lengte_ond.rds"))

tab_ond_lengte |>
  ungroup() |>
  filter(
    jaar == 'jaar 2025',
    v1 == 'langer dan 10 jaar'
  ) |>
  select(markt, aandeel) |>
  write_rds("07 quarto/03 data/tab_ond_lengte10.rds")


#vaste plek sol
tab_ond_plek <- tabel_respons[["ond_plek"]]

levels_markt |>
  map(\(x) {
    filter(tab_ond_plek, markt %in% c(x, "alle markten")) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_rev(jaar),
        fillvar = fct_rev(v2),
        color_pal = discreet[c(10, 7, 1)]
      ) +
      guides(color = 'none', fill = guide_legend(ncol = 1, reverse = T)) +
      facet_wrap(~ fct_relevel(markt, "alle markten", after = Inf))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_v0_vast.rds")


 [1] "#ec0000" "#ff9100" "#d48fb9" "#fdb0cb" "#ffe600" "#bed200" "#6cbd74"
 [8] "#009dec" "#004699" "#e6e6e6"

tab_ond_plek |>
  ungroup() |>
  filter(
    jaar == 'jaar 2025',
    v2 == 'vergunninghouder'
  ) |>
  select(markt, aandeel) |>
  write_rds("07 quarto/03 data/tab_v0_vast.rds")

# food non food niet als figuur alleen in tekst opnemen
# tab_food <- tabel_respons[["ond_nonfood"]] |>
#   filter(jaar == 'jaar 2025') |>
#   group_by(type_markt2, markt) |>
#   mutate(aandeel = aantal / sum(aantal))
