# Rogier van der Groep:
# 250286 monitor markten 2025

library(tidyverse)
library(haven)
library(janitor)

##############################
### DATA 2026 ----------------
##############################

### veldwerkperiode september december 2025 ---

# inlezen bezoekers
markt_26_bez <- read_sav(
  "01 data raw/data 01 monitor 2026/data bezoekers 2026.sav"
) |>
  as_factor() |>
  clean_names() |>
  add_column(
    jaar = "jaar 2025",
    groep = 'bezoekers',
    methode_def = 'f2f'
  )

# inlezen ondernemers ##
markt_26_ond <- read_sav(
  "01 data raw/data 01 monitor 2026/data ondernemers 2026.sav"
) |>
  as_factor() |>
  clean_names() |>
  add_column(
    jaar = "jaar 2025",
    groep = 'ondernemers',
    methode_def = 'f2f'
  )

############################
### DATA 2022 --------------
############################

# inlezen bezoekers Amsterdam en Weesp
markt_22_bez = list(
  # inlezen bezoekers Amsterdam
  bez_22_ams = read_sav(
    "01 data raw/data 01 monitor 2022/Bezoekers_DEF_OZ_V2.sav"
  ) |>
    as_factor() |>
    clean_names() |>
    add_column(jaar = "jaar 2022", groep = 'bezoekers', methode_def = 'f2f'),

  # inlezen bezoekers Weesp
  bez_22_wsp = read_sav(
    "01 data raw/data 01 monitor 2022/F230352_def dataset_BEZ.sav"
  ) |>
    as_factor() |>
    clean_names() |>
    add_column(jaar = "jaar 2022", groep = 'bezoekers', methode_def = 'f2f')
)

# inlezen passantenbestand Amsterdam en Weesp
markt_22_pas <- list(
  # inlezen passanten weesp 2022
  pas_22_ams = read_sav(
    "01 data raw/data 01 monitor 2022/Passanten_DEF_OZ_V2.sav"
  ) |>
    as_factor() |>
    clean_names() |>
    add_column(jaar = "jaar 2022", groep = 'passanten') |>
    mutate(
      methode_def = case_when(
        methode == 'f2f' ~ 'f2f',
        methode == 'panel' ~ 'panel',
        is.na(methode) ~ 'f2f'
      )
    ),

  # inlezen passanten weesp 2022
  pas_22_wsp = read_sav(
    "01 data raw/data 01 monitor 2022/F230352_def_dataset_PAS.sav"
  ) |>
    as_factor() |>
    clean_names() |>
    add_column(jaar = "jaar 2022", groep = 'passanten', methode_def = 'f2f')
)

markt_22_ond <- list(
  # inlezen ondernemers 2022 ---
  ond_22_ams = read_sav(
    "01 data raw/data 01 monitor 2022/Ondernemers_DEF_OZ.sav"
  ) |>
    as_factor() |>
    clean_names() |>
    add_column(jaar = "jaar 2022", groep = 'ondernemers', methode_def = 'f2f'),

  # inlezen ondernemers 2022 weesp ---
  ond_22_wsp = read_sav(
    "01 data raw/data 01 monitor 2022/F230352_def dataset_ON.sav"
  ) |>
    as_factor() |>
    clean_names() |>
    add_column(jaar = "jaar 2022", groep = 'ondernemers', methode_def = 'f2f')
)


#################
### DATA 2016 ---
#################

# inlezen bezoekersbestand 2016 ---
markt_16_bez =
  read_sav("01 data raw/data 01 monitor 2016/16141 data bezoekers DEF.sav") |>
  as_factor() |>
  clean_names() |>
  add_column(jaar = "jaar 2017", groep = 'bezoekers', methode_def = 'f2f')

# inlezen passantendata 2016 ---
markt_16_pas =
  read_sav("01 data raw/data 01 monitor 2016/16141 data passanten DEF.sav") |>
  as_factor() |>
  clean_names() |>
  add_column(jaar = "jaar 2017", groep = 'passanten', methode_def = 'f2f')

# inlezen ondernemers 2016 ---
markt_16_ond =
  read_sav(
    "01 data raw/data 01 monitor 2016/16141 data ondernemers DEF.sav"
  ) |>
  as_factor() |>
  clean_names() |>
  add_column(jaar = "jaar 2017", groep = 'ondernemers', methode_def = 'f2f')
