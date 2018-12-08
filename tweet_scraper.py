#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  7 10:46:30 2018

@author: peerchristensen
"""

# https://stackoverflow.com/questions/37017903/older-tweets-tweepy-using-python

import got3
import csv
import pandas as pd

max_tweets = 6000

tweetCriteria = got3.manager.TweetCriteria().setUntil("2018-12-07").setQuerySearch("vitaepro").setMaxTweets(max_tweets)

tweets = got3.manager.TweetManager.getTweets(tweetCriteria)

tweet_id = []
date = []
text =  []

for i in range(len(tweets)):
    print(i)
    tweet = tweets[i]
    tweet_id.append(tweet.id)
    text.append(tweet.text)
    date.append(tweet.date)

d = {'tweet_id': tweet_id, 'date': date, 'text': text}
df = pd.DataFrame(data=d)

df.to_csv('vitaepro_tweets.csv', encoding='utf-8')
