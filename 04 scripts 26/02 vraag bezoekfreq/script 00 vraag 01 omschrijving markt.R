# vraag 1 wat karakteriseert de markt  ?

library(tidyverse)
library(openxlsx)
#install.packages("tidytext")
library(tidytext)
#install.packages("stopwords")
library(stopwords)
#install.packages("udpipe")
library(udpipe)
#install.packages("wordcloud2")
library(wordcloud2)


# inlezen ruwe data
## inlezen data
markt_list <- read_rds("03 intermediate/markten_totaal.rds")

## script om my_selection en my_bind_rows in te lezen
source("04 scripts 26/00 scr/script 00 functies.R")
source("04 scripts 26/00 scr/script 00 plot functies.R")


#####
model_nl <- udpipe_download_model(language = "dutch")
ud_model_nl <- udpipe_load_model(model_nl$file_model)

model_en <- udpipe_download_model(language = "english")
ud_model_en <- udpipe_load_model(model_en$file_model)

text_nl <- markt_list[["26_bez"]] |>
  filter(a == 'Nederlands') |>
  select("v1") |>
  pull()

text_en <- markt_list[["26_bez"]] |>
  filter(a == 'Engels') |>
  select("v1") |>
  pull()


text_df_id_nl <- markt_list[["26_bez"]] |>
  filter(a == 'Nederlands') |>
  select(markt) |>
  mutate(doc_id = glue::glue("doc{ seq_along(text_nl) }"))

text_df_id_en <- markt_list[["26_bez"]] |>
  filter(a == 'Engels') |>
  select(markt) |>
  mutate(doc_id = glue::glue("doc{ seq_along(text_en) }"))


anno_nl <- udpipe_annotate(
  ud_model_nl,
  x = text_nl
)

anno_en <- udpipe_annotate(
  ud_model_en,
  x = text_en
)

anno_df_nl <- as_tibble(anno_nl) |>
  left_join(text_df_id_nl, by = "doc_id") |>
  filter(!upos %in% c('PUNCT', 'ADP', "ADV", "CCONJ", "SCONJ", "AUX")) |>
  filter(!is.na(lemma)) |>
  mutate(lemma = str_to_lower(lemma))

anno_df_en <- as_tibble(anno_en) |>
  left_join(text_df_id_en, by = "doc_id") |>
  filter(!upos %in% c('PUNCT', 'ADP', "ADV", "CCONJ", "SCONJ", "AUX")) |>
  filter(!is.na(lemma)) |>
  mutate(lemma = str_to_lower(lemma))

anno_df_som <- bind_rows(anno_df_nl, anno_df_en) |>
  mutate(lemma = str_replace_all(lemma, "toerissen", "toerist")) |>
  mutate(lemma = str_replace_all(lemma, "kraamp", "kraampjes")) |>
  mutate(lemma = str_replace_all(lemma, "lijgen", "leeg")) |>

  group_by(markt, lemma) |>
  summarise(aantal = n()) |>
  bind_tf_idf(lemma, markt, aantal) |>
  arrange(desc(tf_idf))

write_rds(anno_df_som, "07 quarto/01 intermediate/tabel_v0_steekwoorden.rds")
