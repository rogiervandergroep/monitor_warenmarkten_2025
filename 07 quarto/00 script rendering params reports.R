library(openxlsx)
params_df <- read.xlsx("07 quarto/markten_quarto.xlsx")


render_one <- function(name, markt) {
  outfile <- glue::glue("report_{ markt }.docx")

  quarto_render(
    input = "factsheet_warenmarkten.qmd",
    execute_params = list(
      markt = markt,
      stadsdeel = sd,
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

outputs <- pmap_chr(params_df, render_one)
