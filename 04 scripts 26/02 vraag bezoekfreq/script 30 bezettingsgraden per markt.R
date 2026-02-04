## bezettingsgraden per markt en totaal

library(tidyverse)
library(lubridate)

data_bezettingsgraden <- openxlsx::read.xlsx(
    "01 data raw/Kopie van Bezettingsgraden.xlsx"
) |>
    pivot_longer(cols = c("2601":"Eindtotaal"))

source("04 scripts 26/00 scr/script 00 plot functies.R")


data_bezettingsgraden |>
    filter(
        name != 'Eindtotaal',
        markt == 'totaal'
    ) |>
    mutate(name = ym(name)) |>
    ungroup() |>
    ggplot(aes(y = value, x = name, group = markt)) +
    geom_line(
        linewidth = 1,
        color = blauw_pal[1]
    ) +
    geom_text(aes(label = label_percent(accuracy = 1)(value)), vjust = -1) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        labels = label_percent(),
        expand = c(0, 0)
    ) +
    labs(y = NULL, x = NULL) +
    theme_os_line(legend_position = 'bottom')
ggsave("06 output figuren/fig_bezettingsgraad.svg", width = 12, height = 6)

data_bezettingsgraden |>
    filter(name == 'Eindtotaal') |>
    fun_totaal_een(
        xvar = round(value * 100),
        yvar = fct_relevel(fct_reorder(Markt_bez, value), 'totaal')
    )
ggsave("06 output figuren/fig_bezettingsgraad2.svg", width = 6, height = 6)


write_rds(data_bezettingsgraden, "03 intermediate/tabel_bezettingsgraden.rds")
