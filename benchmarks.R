
# KPIs and benchmarks 2018

library(tidyverse)
library(lubridate)
library(zoo)
library(tidytext)
library(magrittr)

########## 1. Read data ###################################

# 1. Read data
df <- read_csv("vitaepro_dataCombined.csv") %>%
  filter(time >= ymd("2018-01-01")) # jan 1 - dec 12

# n reviews 
nrow(df)

# % positive and negative reviews
df %>% 
  group_by(rating > 3) %>%
  #count() %>%
  summarise(n = n(),
            n / nrow(df) * 100)
  
# mean sentiment 
df %>% summarise(avg = mean(sentiment),
                 med = median(sentiment))

# % positive mentions of effects

df <- df %>%
  mutate(class = ifelse(rating >= 4,"good","bad"))

my_stopwords <- c("så","vitaepro","pro","vita","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

# find words concerning effects

wordFreq500 <- df %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  count(word,class, sort = TRUE) %>% 
  filter(word != "") %>%
  top_n(500,n) 

include <- c("bedring","produktet","slidgigt", "hjælper", "virkning",
             "ingen", "hjulpet", "virker", "mærker", "føler",
             "effekt", "mærket", "produkt", "forskel", "led",
             "smerter","kroppen","forandring","forbedring","kroppen","rygsmerter",
             "virke","smerte","resultat","resultater","ondt","øm","ømhed","mærkbar","gigt")

effMentions <- df %>% 
  unnest_tokens(word, review) %>%
  filter(word %in% include) %>%
  distinct_at(vars(id,time,class))

effMentions %>%
  group_by(class) %>%
  summarise(n = n(),
            pct = n / nrow(effMentions) * 100)

