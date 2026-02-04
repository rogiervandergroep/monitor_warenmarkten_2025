# open antwoorden v9 reden ontevreden en v10 wat mist

markt_list <- read_rds("03 intermediate/markten_totaal.rds")

openvraag <- markt_list[["26_bez"]] |>
  select(markt, v9_other4, v10_other14, v10_other15, v12_other6, v12_other1)

readr::write_rds(openvraag, "03 intermediate/tab_v9_v12_openant.rds")
