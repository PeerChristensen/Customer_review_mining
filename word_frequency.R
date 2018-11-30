# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Frequency

########## CONTENTS #######################################

# 1. Prepare data
# 2. word frequency
# 3. word cloud
# 4. bigram frequency
# 5. bigram wordcloud
# 6. Word clusters

library(tidyverse)
library(tidytext)
library(magrittr)
library(tm)
library(wordcloud)
library(quanteda)

########## 1. Prepare data ################################

df <- read_csv("vitaepro_data2.csv") %>%
  #mutate(id = paste0(id,time)) %>%
  select(-id,-company,-time,-rating, -sentiment) 

my_stopwords <- c("så","vitaepro","pro","vita","danmark","vitae","vitapro", "vita", "kan",
                  tm::stopwords("danish"))

wordFreq <- df %>%
  unnest_tokens(word, review) %>%
  mutate(word = removeWords(word,my_stopwords)) %>%
  count(word, sort = TRUE) %>%
  filter(word != "")

#totalFreq <- wordFreq %>% 
#  group_by(id) %>% 
#  summarise(total = sum(n))

#wordFreq <- left_join(wordFreq, totalFreq)

#wordFreq <- wordFreq %>%
#  bind_tf_idf(word, id, n)

#words with high td_idf
#wordFreq %>%
  #select(total)  %>% 
#  arrange(desc(tf_idf))

########## # 2. word frequency ############################

# TOP 25

wordFreqTop25 <- wordFreq %>%
  top_n(25,n) %>% 
  arrange(n) %>%
  mutate(row = row_number()) 

wordFreqTop25 %>% 
  ggplot(aes(row, n, fill = log(n))) +
  geom_col(show.legend = FALSE) +
  labs(y = "Frekvens",x="") +
  coord_flip() +
  scale_x_continuous( 
    breaks = wordFreqTop25$row,
    labels = wordFreqTop25$word) +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
      axis.title   = element_text(size = 18),
      axis.title.x = element_text(margin = margin(t = 20,b=10)),
      axis.title.y = element_text(margin = margin(r = 40,l=40)),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Blues")[3],high=brewer.pal(9,"Blues")[9])

ggsave("wordFreqTop25.png")

########## 3. word cloud ##################################

# min = 10 

wordcloud(words = wordFreq$word, freq = wordFreq$n, min.freq = 10, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])

########## 4. bigram frequency ############################

df2 <- read_csv("vitaepro_data2.csv") %>%
  mutate(id = paste0(id,time)) %>%
  select(-company,-time,-rating, -sentiment) 

bigramFreq <- df2 %>%
  unnest_tokens(bigram, review, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stopwords,
         !word2 %in% my_stopwords) %>%
  mutate(word1 = recode(word1,"mdr" = "måneder"),
         word2 = recode(word2,"mdr" = "måneder")) %>%
  mutate(bigram = paste(word1,word2)) %>%
  count(bigram, sort = TRUE)

# TOP 25

bigramFreqTop25 <- bigramFreq %>%
  top_n(25,n) %>% 
  arrange(n) %>%
  mutate(row = row_number()) 

bigramFreqTop25 %>% 
  ggplot(aes(row, n, fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Frekvens",x="") +
  coord_flip() +
  scale_x_continuous( 
    breaks = bigramFreqTop25$row,
    labels = bigramFreqTop25$bigram) +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 40,l=40)),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_fill_gradient(low=brewer.pal(9,"Blues")[2],high=brewer.pal(9,"Blues")[9])

ggsave("bigramFreqTop25.png")

########## 5. bigram word cloud ##################################

# min = 10 

wordcloud(words = bigramFreq$bigram, freq = bigramFreq$n, min.freq = 3, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(9,"Blues")[4:9])


