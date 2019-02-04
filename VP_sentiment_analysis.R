# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Sentiments

########## CONTENTS #######################################

# 1. Prepare data
# 2. Examples
# 3. Sentiment distribution + best and worst
# 4. anomaly detection
# 5. correlation

library(tidyverse)
library(tidytext)
library(magrittr)
library(tm)
library(wordcloud)
library(happyorsad)
library(ggridges)

########## 1. Prepare data ################################

df <- read_csv("vitaepro_dataCombined.csv") %>%
  select(-company)

########## 2. Examples ####################################

cat("Det er et fantastisk produkt."," - score:",
    happyorsad("Det er et fantastisk produkt"))
# score = 4
cat("Elendig service, og pillerne smager dårligt."," - score:",
    happyorsad("Elendig service, og pillerne smager dårligt."))
# score = -6

########## 3. Sentiment distribution ######################

# density plot

df %>% 
  ggplot(aes(x=sentiment)) + 
  geom_density(size=1, colour = "steelblue") +
  geom_vline(xintercept = median(df$sentiment), 
             colour = "goldenrod3", linetype = "dashed", size =1) +
  ggplot2::annotate("text", x = 10, y = 0.15, 
                    label = paste("median = ", median(df$sentiment)), colour = "goldenrod3", size = 7) +
  xlim(-20,20) +
  labs(x="Sentiment score") +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_blank(),
        panel.grid = element_blank())

ggsave("sentiment_distribution.png")

# density plot by rating

df %>%
  ggplot(aes(x=sentiment,y=factor(rating),fill=factor(rating))) +
  geom_density_ridges(rel_min_height = 0.005, alpha=.8) +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"),guide=F) +
  labs(x="Sentiment score") +
  theme_minimal() + 
  theme(axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_blank())

ggsave("sentiment_distribution_by_rating.png")

# best and worst reviews

df %>% 
  filter(sentiment == max(sentiment) | sentiment == min(sentiment))

# every review sentiment + rating

df %>%
  mutate(index = rev(row_number())) %>%
  ggplot(aes(x=index,y=sentiment,fill=factor(rating))) +
  geom_col() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"), guide = F) +
  theme_minimal() +
  labs(y="Sentiment Score") +
  theme(panel.grid = element_blank(),
        axis.text  = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10)))

ggsave("reviews_sentiment_rating_barsUpdate.png", width= 20,height=10)        

########## 4. Anomaly detection ###########################

# to fejlratings

df %>% 
  filter(rating <= 2, sentiment >= 4) %>%
  write_csv("evt_fejlratings.csv")

########## 5. Correlation #################################

df %>% select(rating,sentiment) %>% cor()



