setwd("C:/Users/Chris/Desktop/FWYF-hack/")
library(tidytext)
library(tidyverse)
library(data.table)

lyrics <- as.data.frame(fread("lyrics_final2.csv", sep = ","))

lyric_words <- lyrics %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song) %>%
  mutate(word_position = row_number()/n())

lyric_words <- lyric_words %>%
  anti_join(stop_words)

write_csv(lyric_words, "lyric_words.csv")
