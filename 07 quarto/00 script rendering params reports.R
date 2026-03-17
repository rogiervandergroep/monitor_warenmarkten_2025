library(openxlsx)
# install.packages("quarto")
library(quarto)
library(tidyverse)
library(purrr)


# inlezen alle markten
params_df <- read.xlsx("07 quarto/markten_quarto.xlsx")


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

# render_one(
#   markt = "Lambertus Zijlplein",
#   stadsdeel = 'Nieuw-West',
#   type = "eendaagse markt",
#   map_id = 10
# )

# render_one(
#   markt = "Tussen Meer",
#   stadsdeel = 'Nieuw-West',
#   type = "eendaagse markt",
#   map_id = 14
# )

# render_one(
#   markt = "Albert Cuypmarkt",
#   stadsdeel = 'Zuid',
#   type = "markt op meerdere dagen",
#   map_id = 2
# )

# render_one(
#   markt = "Ten Katemarkt",
#   stadsdeel = 'West',
#   type = "markt op meerdere dagen",
#   map_id = 17
# )

# render_one(
#   markt = "Buikslotermeerplein",
#   stadsdeel = 'Noord',
#   type = "markt op meerdere dagen",
#   map_id = 3
# )

# render_one(
#   markt = "Dappermarkt",
#   stadsdeel = 'Oost',
#   type = "markt op meerdere dagen",
#   map_id = 4
# )
