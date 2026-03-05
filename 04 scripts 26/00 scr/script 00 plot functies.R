#### figuren ----

library(scales)

source(
  'http://gitlab.com/os-amsterdam/tools-onderzoek-en-statistiek/-/raw/main/R/load_all.R'
)


grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"

stoplicht6 <- c(
  "#e6e6e6",
  "#ec0000",
  "#f28b21",
  "#f8d376",
  "#e2e693",
  "#a1cd73",
  "#53b361"
)
os_blauw <- c(
  "#e6e6e6",
  "#e7e8f4",
  "#d0d2e8",
  "#b8bcdd",
  "#a1a7d2",
  "#8992c6",
  "#707ebb",
  "#566bb0",
  "#3858a4",
  "#004699"
)

discreet <- c(
  "#ec0000",
  "#ff9100",
  "#d48fb9",
  "#fdb0cb",
  "#ffe600",
  "#bed200",
  "#6cbd74",
  "#009dec",
  "#004699",
  "#e6e6e6"
)


# figuur totaal: onderscheid dagmarkt en overige markten -
fun_totaal <- function(
  x,
  yvar,
  xvar,
  fillvar,
  color_pal,
  nr = 2,
  positie = 'stack'
) {
  hcl <- farver::decode_colour(color_pal, "rgb", "hcl")

  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

  x |>
    ggplot(aes(y = {{ yvar }}, x = {{ xvar }}, fill = {{ fillvar }})) +
    geom_col(position = positie) +
    geom_text(
      aes(
        label = if_else({{ xvar }} > 0.04, round({{ xvar }} * 100), NA),
        color = {{ fillvar }}
      ),
      size = 4.5,
      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8
    ) +
    labs(y = NULL, x = NULL) +
    theme_os(orientation = 'horizontal') +
    theme(text = element_text(size = 16)) +
    scale_fill_manual(name = NULL, values = color_pal) +
    scale_color_manual(name = NULL, values = label_col) +
    scale_x_continuous(labels = scales::percent) +
    guides(
      color = 'none',
      fill = guide_legend(nrow = nr, reverse = T)
    )
}


# handig voor kolommen zonder fill

# verm_factor is de vermenigingsvuldigingsfactor: 100 of 1

fun_totaal_een <- function(
  x,
  xvar,
  yvar,
  grenswaarde = 100,
  verm_factor = 100,
  afr = 0
) {
  x |>

    ggplot(aes(
      y = {{ yvar }},
      x = {{ xvar }}
    )) +

    geom_col(fill = blauw_pal[2]) +

    geom_text(
      aes(
        label = if_else(
          {{ xvar }} < grenswaarde,
          NA,
          round({{ xvar }} * verm_factor, afr)
        )
      ),
      hjust = 1.5,
      size = 4.5,
      family = font,
      color = "white",
      lineheight = .8
    ) +

    labs(title = NULL, x = NULL, y = NULL) +
    theme_os(orientation = 'horizontal') +
    theme(text = element_text(size = 16)) +
    scale_fill_manual(name = NULL, values = blauw_pal[2]) +
    guides(fill = guide_legend(reverse = T))
}


theme_os_line <- function(
  legend_position = "bottom",
  drop_axis_titles = F
) {
  grDevices::windowsFonts(
    `Amsterdam Sans` = grDevices::windowsFont("Amsterdam Sans")
  )
  font <- "Amsterdam Sans"
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        family = font,
        size = 13,
        face = "bold"
      ),
      plot.caption = ggplot2::element_text(family = font, size = 14),
      axis.title = ggplot2::element_text(
        family = font,
        hjust = 1,
        size = 13
      ),
      plot.subtitle = ggplot2::element_text(family = font, size = 15),
      legend.text = ggplot2::element_text(family = font, size = 12),
      plot.title = ggplot2::element_text(
        family = font,
        lineheight = 1.2,
        size = 15
      ),
      panel.grid.major.x = element_blank(),
      axis.line.x = element_line(colour = "black", linewidth = 0.6),
      axis.text.x = element_text(margin = margin(t = 10)),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = legend_position,
      panel.border = ggplot2::element_rect(
        fill = "transparent",
        color = NA
      ),
      strip.text = ggplot2::element_text(
        color = "black",
        family = font,
        face = "bold",
        size = 15
      )
    )
  return(theme)
}
