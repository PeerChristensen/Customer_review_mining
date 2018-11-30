# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Overview of the data

########## CONTENTS #######################################

# 1. Read data
# 2. N reviews
# 3. N reviews by month
# 4. Average rating by month
# 5. Distribution of ratings
# 6. Distribution of ratings by quarter

library(tidyverse)
library(lubridate)
library(zoo)

########## 1. Read data ###################################

# 1. Read data
df <- read_csv("vitaepro_data2.csv")

########## 2. N reviews ###################################

nrow(df) # 678, excluding reviews from before july 2017

########## 3. N reviews by month ##########################

df %>%
  group_by(month=floor_date(time, "month")) %>%
  summarise(n = n()) %>%
  mutate(cumsum = cumsum(n)) %>%
  ggplot(aes(x=month,y=cumsum)) +
  geom_line(size=1) +
  geom_point(size=2) +
  theme_minimal() +
  labs(y="Antal anmeldelser akk.",x="Måned") +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10))) 

ggsave("reviews_by_month_acc.png")

########## 4. Average rating by month #####################

df %>%
  group_by(month=floor_date(time, "month")) %>% 
  summarise(n  = n(),
            m  = mean(rating),
            sd = sd(rating)) %>%
  filter(month >= date("2018-01-01")) %>%
  ggplot(aes(month,m)) +
  geom_line(size=1) +
  geom_point(aes(month,m,colour=m), size = 3) +
  geom_errorbar(aes(ymin=m-sd,ymax=m+sd),width=3) +
  scale_colour_gradient(low="#73CF11", high = "#00B67A",guide=F) +
  ylim(0,6) +
  labs(y="Gennemsnitlig rating",x="Måned") +
  theme_minimal() +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10)),
        axis.title.y = element_text(margin = margin(r = 20,l=10))) 

ggsave("avg_rating_by_month.png")

########## 5. Distribution of ratings #####################

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

ggsave("rating_distribution.png")

########## 6. Distribution of ratings by quarter ##########

quarters <- df %>% 
  group_by(quarter = floor_date(time, "quarter")) %>%
  count(rating) %>%
  ungroup() %>%
  complete(quarter, nesting(rating), fill = list(n =0)) %>%
  group_by(quarter) %>%
  mutate(prop  = n / sum(n),
         order = rev(row_number()))

quarters$Kvartal <- (as.yearqtr(quarters$quarter, format = "%Y-%m-%d"))

quarters %>%
  ggplot(aes(x=factor(Kvartal), y = prop, fill = factor(rating), group=reorder(rating,order))) +
  geom_col() +
  scale_fill_manual(values = c("#FF3722","#FF8622","#FFCE00","#73CF11","#00B67A"), guide = F) +
  theme_minimal() +
  labs(y="",x="Kvartal") +
  theme(axis.text    = element_text(size = 16),
        axis.title   = element_text(size = 18),
        axis.title.x = element_text(margin = margin(t = 20,b=10))) 

ggsave("ratings_ditribution_kvartal.png")








