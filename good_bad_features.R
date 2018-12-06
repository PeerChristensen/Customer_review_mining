# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: GOOD / BAD FEATURES

########## CONTENTS #######################################

# 1. Prepare data

library(tidyverse)
library(tidytext)
library(magrittr)
library(tm)
library(wordcloud)
library(happyorsad)
library(ggrepel)

########## 1. Prepare data ################################

df <- read_csv("vitaepro_data2.csv") %>%
  #mutate(id = paste0(id,time)) %>%
  select(-id,-company,-time,rating, -sentiment) %>%
  mutate(class = ifelse(rating>=4,"good","bad"))

my_stopwords <- c("så","vitaepro","pro","vita","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

wordFreq500 <- df %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  count(word,class, sort = TRUE) %>% 
  filter(word != "") %>%
  top_n(500,n) 

good <- wordFreq500 %>% filter(class=="good")
bad  <- wordFreq500 %>% filter(class=="bad")

goodBad <- inner_join(good,bad, by = "word")

nGood <- df %>% filter(class=="good") %>% count()
nBad <- df %>% filter(class=="bad") %>% count()

include <- c("bedring","produktet", "service","slidgigt","lovet","måske","tilfreds", "hjælper", "virkning",
             "ingen", "hjulpet", "tid", "virker", "mærker", "føler", "kundeservice", "levering",
             "rigtig","effekt", "mærket", "produkt", "forskel", "led","måned", "mdr", "vitaminer",
             "smerter","anbefale","tvivl","oplevelse","levering","kroppen")

goodBad %<>% 
  mutate(nGood = as.numeric(nGood), nBad = as.numeric(nBad)) %>%
  mutate(probGood = n.x / nGood,
         probBad = n.y / nBad) %>%
  filter(n.y>=3, n.x>=3) %>%
  filter(word %in% include)

goodBad %>%
  ggplot(aes(x=probGood,y=probBad,label = word)) +
  ylim(c(0,.2)) +
  xlim(c(0,.2)) +
  geom_point(size=2) +
  geom_abline(intercept = 0, slope = 1, colour = "steelblue") +
  geom_text_repel(size=5) +
  labs(x="Sandsynlighed: God anmeldelse",
       y="Sandsynlighed: Dårlig anmeldelse") +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 30,b=10)),
        axis.title.y = element_text(margin = margin(r = 30,l=10)),
        panel.grid = element_blank())

ggsave("good_bad_correlation.png")  
