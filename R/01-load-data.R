# Load data for training and evaluation


library(tidyverse)
library(purrr)
library(twitteR)
source("twitterauth.R")


# David Robinson's data
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

# Get newer tweets 
# You'd need to set global options with an authenticated app
setup_twitter_oauth(twitter_consumer_key,
                    twitter_consumer_secret,
                    twitter_access_token,
                    twitter_access_token_secret)

# We can request only 3200 tweets at a time; it will return fewer
# depending on the API
trump_tweets_new <- userTimeline("realDonaldTrump", n = 3200)
trump_tweets_new <- tbl_df(map_df(trump_tweets_new, as.data.frame))

missing_tweets <- userTimeline("realDonaldTrump", n = 3200, maxID = trump_tweets_new$id[nrow(trump_tweets_new)])
missing_tweets <- tbl_df(map_df(missing_tweets, as.data.frame))

more_missing <- userTimeline("realDonaldTrump", n = 3200, maxID = missing_tweets$id[nrow(missing_tweets)])
more_missing <- tbl_df(map_df(more_missing, as.data.frame))


alltweets <- rbind(trump_tweets_df, more_missing, missing_tweets, trump_tweets_new) %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

alltweets <- alltweets[!duplicated(alltweets$id),]

save(alltweets, file = "../data/alltweets.Rda")
