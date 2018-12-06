Sys.setlocale(locale = 'en_US.UTF-8')

# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Wellvita

########## Contents ###################

# 1. Scrape the data
# 2. Clean the data
# 3. Write csv file

########## LOAD PACKAGES ###############

library(tidyverse)
library(trustpilotR)
library(magrittr)
library(happyorsad)
library(zoo)
library(lubridate)
library(widyr)
library(ggraph)
library(igraph)

########## SCRAPE DATA #################

#NOTE that Trustpilot have changed their site
# you now need to set page_lim

#df <- get_reviews("https://dk.trustpilot.com/review/www.wellvita.dk", page_lim=250,company = "Wellvita")


########## CLEAN DATA #########

df <- read_csv("wellvita_data.csv")

# lower case, add sentiments
df %<>%
  mutate(review = tolower(review)) %>%              
  mutate(sentiment = map_int(df$review,happyorsad,"da"))

########## WRITE CSV FILE ###############

#write_csv(df,"wellvita_data.csv")


# rating distribution
df %>% 
  group_by(rating) %>%
  count() %>%
  ggplot(aes(x=factor(rating),y=n,fill=factor(rating))) + 
  geom_col() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"),guide=F) +
  labs(y="Antal",x="Rating") +
  theme_minimal() +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10))) 

ggsave("wellvita_distr_ratings_png")

#### subset movizin

df2 <- df %>%
  filter(str_detect(review,"movizin"))

df2 %>% 
  group_by(rating) %>%
  count() %>%
  ggplot(aes(x=factor(rating),y=n,fill=factor(rating))) + 
  geom_col() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"),guide=F) +
  labs(y="Antal",x="Rating") +
  theme_minimal() +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10))) 

ggsave("movizin_distr_ratings_png")

## word co-occurrence

my_stopwords <- c("så","movizin","wellvita","vita","1","d","2","3","venlig","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

df %<>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  add_count(word)                          %>%
  filter(n > 1,word != "")

word_pairs <- df %>%
  pairwise_count(word, id, sort = TRUE)

# word_pairs_neg <- df %>%
#   filter(rating < 4) %>%
#   pairwise_count(word, id, sort = TRUE)

########## 1. Plot ########################################

set.seed(611)

pairs_plot <- word_pairs %>%
  filter(n > 60)                  %>%
  graph_from_data_frame()        %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "steelblue",show.legend=F) +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

pairs_plot

ggsave("wellvita_cooccurrence.png")
