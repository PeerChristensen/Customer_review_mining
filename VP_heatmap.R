# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Heatmaps

########## CONTENTS #######################################

# Negative reviews #

# posivite reviews #

library(tidyverse)
library(tidytext)
library(magrittr)
library(tm)
library(ggrepel)
library(stm)
library(furrr)
library(ggrepel)
library(RColorBrewer)

########## 1. NEGATIVE REVIEWS ############################

dfNeg <- read_csv("vitaepro_data2.csv")

dfNeg %<>%
  filter(rating < 4) %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "") %>%                
  select(-n, -sentiment,-time,-company,-rating)

dfSparseNeg <- dfNeg             %>%
  count(id, word)          %>%
  cast_sparse(id, word, n)

neg_model <- stm(dfSparseNeg, K = 5, verbose = TRUE)

posteriorNeg <- neg_model$theta  %>% 
  data.frame() %>%
  mutate(id = unique(df$id))

posteriorNeg                       %<>% 
  gather(topic, value, -id)      %>%
  mutate(topic = as.factor(as.numeric(str_replace(topic,"X",""))),
         id = factor(id)) %>%
  as_tibble()

posteriorNeg$id <- factor(posteriorNeg$id, levels = unique(df$id))

posteriorNeg %>% 
  ggplot(aes(topic,fct_rev(id))) + 
  geom_tile(aes(fill = value), colour = "snow") + 
  #scale_fill_stellenbosch(discrete = F, "wine") +
  scale_fill_gradient(low = "snow", high = "#FF3722",guide=F) +
  theme_minimal() +
  theme(axis.text.y    = element_text(size = 2),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_blank()) 

ggsave("heatmap_negative.png")

########## 1. POSITIVE REVIEWS ############################

dfPos <- read_csv("vitaepro_data2.csv")

dfPos %<>%
  filter(rating >= 4) %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "") %>%                
  select(-n, -sentiment,-time,-company,-rating)

dfSparsePos <- dfPos       %>%
  count(id, word)          %>%
  cast_sparse(id, word, n)

pos_model <- stm(dfSparsePos, K = 6, verbose = TRUE)

posteriorPos <- pos_model$theta  %>% 
  data.frame() %>%
  mutate(id = unique(dfPos$id))

posteriorPos                       %<>% 
  gather(topic, value, -id)      %>%
  mutate(topic = as.factor(as.numeric(str_replace(topic,"X",""))),
         id = factor(id)) %>%
  as_tibble()

posteriorPos$id <- factor(posteriorPos$id, levels = unique(dfPos$id))

posteriorPos %>% ggplot(aes(topic,fct_rev(id))) + 
  geom_tile(aes(fill = value), colour = "snow") + 
  #scale_fill_stellenbosch(discrete = F, "wine") +
  scale_fill_gradient(low = "snow", high = "#00B67A",guide=F) +
  theme_minimal() +
  theme(axis.text.y    = element_text(size = 2),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_blank()) 

ggsave("heatmap_positive.png")

