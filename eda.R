setwd("FWYF-hack/")
library(tidytext)
library(tidyverse)
library(data.table)

lyrics <- as.data.frame(fread("lyrics.csv", sep = ","))

artists <- lyrics %>%
  group_by(artist) %>%
  summarise(counts = n()) %>%
  arrange(-counts) %>%
  filter(counts >= 100)

lyrics <- lyrics %>%
  filter(artist %in% artists$artist) %>%
  select(-index)

write_csv(lyrics, "lyrics_final2.csv")

lyric_words <- lyrics %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song) %>%
  mutate(word_position = row_number()/n())

# lyric_words <-