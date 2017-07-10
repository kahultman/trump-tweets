# Collect streaming data
library(tidyverse)
library(streamR)
load("my_oauth.Rdata")
filterStream(file.name = "tweet_stream.json", 
             follow = "25073877",
             tweets = 10,
             oauth = my_oauth, 
             timeout = 60)

tweet_stream.df <- parseTweets("tweet_stream.json", simplify = TRUE)

trump_tweet_stream.df <- filter(tweet_stream.df, user_id_str == "25073877") 
