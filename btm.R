# BTM

library(BTM)
library(udpipe)
library(tidyverse)
library(magrittr)
library(tidytext)
library(tm)

df <- read_csv("vitaepro_data2.csv")

my_stopwords <- c("så","vitaepro","pro","vita","2","3","venlig","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

df %<>%
  filter(rating >= 4) %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "") %>%                
  select(-n, -sentiment,-time,-company,-rating)

udmodel <- udpipe_load_model("danish-ud-2.0-170801.udpipe")

include <- udpipe(x = df$word,
                  object = udmodel)

include <- include      %>%
  select(token,upos)    %>%
  filter(upos %in% c("NOUN")) %>%
  select(token) 

df <- df %>%
  filter(word %in% include$token)

## Building the model
set.seed(321)
model  <- BTM(df, k = 4, beta = 0.01, iter = 1000, trace = 100)

## Inspect the model - topic frequency + conditional term probabilities
model$theta

topicterms <- terms(model, top_n = 5)
topicterms
