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

reklame <- df %>%
  filter(grepl("reklame",text)==T) %>% 
  mutate(sentiment = map_int(text,happyorsad))

df %>%
  group_by(month=floor_date(date, "month")) %>%
  count() %>%
  filter(year(month) >= 2016) %>% 
  ggplot(aes(x=month,y=n)) +
  geom_col(fill="steelblue") +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank(),
        strip.text.x = element_text(size=16))

ggsave("tweets.png")
