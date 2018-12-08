# Project: Text mining VitaePro reviews
# Author: Peer Christensen
# Date: December, 2018
# Task: Twitter data
# 
# https://rtweet.info/
#   
#   https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#   
#   https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html
# 
# library(rtweet)
# 
# 
# ## autheticate via web browser
# token <- create_token(
#   app = "textR",
#   consumer_key = "hrztENrNCQ5FpXKK1RdeN8zFf",
#   consumer_secret = "GE9WUVCt4mDpmT6u2SNif5rxbzG3DPK0gBZpu7tz1TopOlQYnE")
# 
# tweets <- search_tweets2(
#   "Vitaepro", n = 100000, include_rts = FALSE,
#   retryonratelimit = TRUE
# )

library(tidyverse)
library(magrittr)
library(cldr)

df <- read_csv("vitaepro_tweets.csv")

language <- detectLanguage(df$text)

df %<>% 
  select(-X1) %>%
  mutate(tweet_id = as.character(tweet_id),
         language = language$detectedLanguage)

df[grep("Åge | Hareide",df$text),]

df[grep("reklame",df$text),]

