#### bezettingsgraden ---
source("07 quarto/00 scripts/script 00 plot functies.R")


bezettingsgraden <- read_rds("03 intermediate/tabel_bezettingsgraden.rds")


levels_markt |>
  map(\(x) {
    filter(
      bezettingsgraden,
      name != 'Eindtotaal',
      markt %in% c(x, "totaal")
    ) |>
      mutate(name = ym(name)) |>
      ggplot(aes(
        y = value,
        x = name,
        color = fct_relevel(markt, 'totaal', after = Inf)
      )) +
      geom_line(linewidth = 1) +
      geom_text(
        aes(label = label_percent(accuracy = 1)(value)),
        vjust = -1,
        size = 5
      ) +
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
