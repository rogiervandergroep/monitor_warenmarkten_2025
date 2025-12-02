#### figuren ----

grDevices::windowsFonts(
  "Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans")
)
font <- "Amsterdam Sans"

#stoplicht6<- c("#ec0000", "#f28b21", "#f8d376", "#e2e693", "#a1cd73", "#53b361")

os_blauw <- c(
  "#e6e6e6",
  "#dcddee",
  "#b8bcdd",
  "#959dcc",
  "#707ebb",
  "#4861aa",
  "#004699"
)


# figuur totaal: onderscheid dagmarkt en overige markten -
fun_totaal <- function(
  x,
  yvar,
  xvar,
  fillvar,
  color_pal,
  positie = 'stack'
) {
  hcl <- farver::decode_colour(color_pal, "rgb", "hcl")

  label_col <- ifelse(hcl[, "l"] > 50, "black", "white")

  x |>
    ggplot(aes(y = {{ yvar }}, x = {{ xvar }}, fill = {{ fillvar }})) +
    geom_col(position = positie) +
    geom_text(
      aes(
        label = if_else(
          round({{ xvar }}) > 4,
          round({{ xvar }}),
          NA
        ),
        color = {{ fillvar }}
      ),
      position = position_stack(vjust = 0.5),
      family = font,
      lineheight = .8
    ) +
    labs(y = NULL, x = NULL) +
    theme_os() +
    scale_fill_manual(name = NULL, values = color_pal) +
    scale_color_manual(name = NULL, values = label_col) +
    guides(
      color = 'none',
      fill = guide_legend(nrow = 2, reverse = T)
    )
}
