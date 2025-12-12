# welke andere markten v14_a
# welke andere winkelgebieden v14_b

### Welke andere markten v14a

# 9. Waarom bent u ontevreden over het productaanbod?

library(tidyverse)
library(openxlsx)

# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

markt_list[["26_bez"]] <- markt_list[["26_bez"]] |>
  mutate(
    v14_a1 = case_when(
      is.na(v14_a1) ~ v14_a_codes,
      TRUE ~ v14_a1
    )
  )


function_vraag <- function(x, group_vars) {
  bind_rows(
    x |>
      group_by(v14 = v14_a1, across(all_of(group_vars))) |>
      summarise(aantal = n()),

    x |>
      group_by(v14 = v14_a2, across(all_of(group_vars))) |>
      summarise(aantal = n()),

    x |>
      group_by(v14 = v14_a3, across(all_of(group_vars))) |>
      summarise(aantal = n())
  ) |>
    group_by(v14, across(all_of(group_vars))) |>
    summarise(aantal = sum(aantal)) |>
    filter(!is.na(v14)) |>
    group_by(across(all_of(group_vars))) |>
    mutate(aandeel = aantal / sum(aantal))
}


df_v14a_anderemarkten_totaal <- markt_list[["26_bez"]] |>
  function_vraag(group_vars = NULL)


df_v14a_anderemarkten <- markt_list[["26_bez"]] |>
  function_vraag(group_vars = c("markt", "stadsdeel_markt", "type_markt2"))


df_v14a_anderemarkten_selectie <- bind_rows(
  df_v14a_anderemarkten |>
    filter(v14 != 'geen andere markten') |>
    group_by(markt) |>
    slice_max(aandeel, n = 2),

  df_v14a_anderemarkten |>
    filter(v14 == 'geen andere markten')
)
