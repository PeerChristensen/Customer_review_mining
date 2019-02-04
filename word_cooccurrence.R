# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Word co-occurrence

########## CONTENTS #######################################

# 1. Prepare data
# 2. Plot 

library(tidyverse)
library(tidytext)
library(gridExtra)
library(widyr)
library(ggraph)
library(igraph)
library(tm)

########## 1. Prepare data ################################

df <- read_csv("vitaepro_dataCombined.csv")

my_stopwords <- c("så","vitaepro","pro","vita","1","d","2","3","venlig","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

df %<>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "")

word_pairs_pos <- df %>%
  filter(rating >= 4) %>%
  pairwise_count(word, id, sort = TRUE)

word_pairs_neg <- df %>%
  filter(rating < 4) %>%
  pairwise_count(word, id, sort = TRUE)

########## 1. Plot ########################################

set.seed(611)

pairs_plot_pos <- word_pairs_pos %>%
  filter(n > 5)                  %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#00B67A",show.legend=F) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

pairs_plot_neg <- word_pairs_neg %>%
  filter(n >= 4)                 %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#FF3722",show.legend=F) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

grid.arrange(pairs_plot_pos, pairs_plot_neg, ncol = 2)

