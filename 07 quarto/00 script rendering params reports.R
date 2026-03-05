library(openxlsx)
# install.packages("quarto")
library(quarto)
library(tidyverse)
# inlezen alle markten
params_df <- read.xlsx("07 quarto/markten_quarto.xlsx")

# inlezen markten nieuw-west
params_nw <- params_df |>
  filter(markt %in% c("Tussen Meer", "Lambertus Zijlplein"))

## functie
render_one <- function(markt, stadsdeel, type, map_id) {
  outfile <- glue::glue("factsheet_{ markt }.docx")

  quarto_render(
    input = "07 quarto/factsheet_warenmarkten.qmd",
    execute_params = list(
      markt = markt,
      stadsdeel = stadsdeel,
      type = type,
      map_id = map_id
    ),
    output_file = outfile,
    quiet = TRUE
  )

  message("Rendered: ", outfile)

  return(outfile)
}


library(purrr)

render_one(
  markt = "Lambertus Zijlplein",
  stadsdeel = 'Nieuw-West',
  type = "eendaagse markt",
  map_id = 10
)

# Tussen Meer Nieuw-West eendaagse markt     14
