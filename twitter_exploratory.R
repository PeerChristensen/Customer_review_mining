# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Twitter data
# 

library(tidyverse)
library(magrittr)
library(cldr)

df <- read_csv("vitaepro_tweets.csv")

language <- detectLanguage(df$text)

df %<>% 
  select(-X1) %>%
  mutate(tweet_id = as.character(tweet_id),
         language = language$detectedLanguage)

df[grep("Åge | Hareide",df$text),]

df[grep("reklame",df$text),]

