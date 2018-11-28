Sys.setlocale(locale = 'en_US.UTF-8')

# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Scrape, clean and write

########## Contents ###################

# 1. Scrape the data
# 2. Clean the data
# 3. Write csv file

########## LOAD PACKAGES ###############

library(tidyverse)
library(trustpilotR)
library(magrittr)
library(happyorsad)

########## SCRAPE DATA #################

#NOTE that Trustpilot have changed their site
# you now need to set page_lim

#df <- get_reviews("https://dk.trustpilot.com/review/vitaepro.dk", page_lim=35,company = "VitaePro")

########## CLEAN DATA #########

#df <- read_csv("vitaepro_data2.csv")

# too few data points before july 2017, subset
df %<>% 
  filter(time >= "2017-07-01")

# lower case, add sentiments
df %<>%
  mutate(review = tolower(review)) %>%              
  mutate(sentiment = map_int(df$review,happyorsad,"da"))

########## WRITE CSV FILE ###############

#write_csv(df, "vitaepro_data2.csv")

