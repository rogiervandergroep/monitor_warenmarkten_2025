#### bezettingsgraden ---
source("07 quarto/00 scripts/script 00 plot functies.R")


bezettingsgraden <- read_rds("03 intermediate/tabel_bezettingsgraden.rds") |>
  my_markt_rename()


levels_markt |>
  map(\(x) {
    filter(
      bezettingsgraden,
      name != 'Eindtotaal',
      markt %in% c(x, "alle markten")
    ) |>
      mutate(name = ym(name)) |>
      ggplot(aes(
        y = value,
        x = name,
        color = fct_relevel(markt, 'alle markten', after = Inf)
      )) +
      geom_line(linewidth = 1) +
      geom_text(
        aes(label = label_percent(accuracy = 1)(value)),
        vjust = -1,
        size = 5
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(
        limits = c(0, 1.2),
        breaks = seq(0, 1, by = 0.2),
        labels = label_percent(),
        expand = c(0, 0)
      ) +
      labs(y = NULL, x = NULL) +
      scale_color_manual(values = c("#004699", "#6cbd74")) +
      theme_os_line(legend_position = 'bottom') +
      theme(text = element_text(size = 15)) +
      guides(color = 'legend')
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_bezettingsgraden.rds")

### data marktbureau ---
tabel_mb_lft <- read_rds("03 intermediate/tabel_lft_ondernemers_mb.rds")

leeft_per_markt <- tabel_mb_lft[["gem_lft"]] |>
  my_markt_rename()

lft_quant <- tabel_mb_lft[["cat_lft"]] |>
  my_markt_rename()

duur_quant <- tabel_mb_lft[["cat_duur"]] |>
  my_markt_rename()

### getallen voor in de tekst
mb_getallen <- list()

mb_getallen$aandeel_vergun <- leeft_per_markt |>
  filter(type_ondernemer == 'vergunninghouder') |>
  select(markt, aandeel)


mb_getallen$aandeel_60 <- lft_quant |>
  filter(leeftijdsklasse == '60 jaar en ouder') |>
  select(markt, aandeel)

mb_getallen$aandeel_17 <- duur_quant |>
  filter(duur_klasse == '17 jaar of langer') |>
  select(markt, aandeel)

write_rds(mb_getallen, "07 quarto/03 data/tab_mb_getallen.rds")


levels_markt |>
  map(\(x) {
    filter(lft_quant, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, 'alle markten'),
        fill = fct_rev(leeftijdsklasse),
        color_pal = os_blauw[c(2, 4, 6, 9)]
      ) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_mb_lftklas.rds")

levels_markt |>
  map(\(x) {
    filter(leeft_per_markt, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, 'alle markten'),
        fill = type_ondernemer,
        color_pal = discreet[c(7, 4, 1)]
      ) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_mb_type_ond.rds")


levels_markt |>
  map(\(x) {
    filter(duur_quant, markt %in% c(x, 'alle markten')) |>
      fun_totaal(
        xvar = aandeel,
        yvar = fct_relevel(markt, 'alle markten'),
        fill = fct_rev(duur_klasse),
        color_pal = os_blauw[c(2, 4, 6, 9)]
      ) +
      guides(color = 'none', fill = guide_legend(nrow = 2, reverse = T))
  }) |>
  set_names(levels_markt) |>
  write_rds("07 quarto/02 figuren/fig_mb_duurklas.rds")
