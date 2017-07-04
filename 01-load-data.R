# Load data for training and evaluation


library(tidyverse)
library(purrr)
library(twitteR)
source("twitterauth.R")


# David Robinson's data
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

tweets <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

# Get newer tweets 
# You'd need to set global options with an authenticated app
setup_twitter_oauth(twitter_consumer_key,
                    twitter_consumer_secret,
                    twitter_access_token,
                    twitter_access_token_secret)

# We can request only 3200 tweets at a time; it will return fewer
# depending on the API
trump_tweets_new <- userTimeline("realDonaldTrump", n = 3200)
trump_tweets_new_df <- tbl_df(map_df(trump_tweets_new, as.data.frame))


missing_tweets <- userTimeline("realDonaldTrump", n = 3200, maxID = "797812048805695488")
missing_tweets_df <- tbl_df(map_df(missing_tweets, as.data.frame))

more_missing <- userTimeline("realDonaldTrump", n = 3200, maxID = "764053208394182656")
more_missing_df <- tbl_df(map_df(more_missing, as.data.frame))


missing_tweets_df <- rbind(missing_tweets_df, more_missing_df)
missing_tweets_df <- rbind(trump_tweets_new_df, missing_tweets_df)

missing_tweets <- missing_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

alltweets <- rbind(tweets, missing_tweets) 
alltweets <- alltweets[!duplicated(alltweets$id),]

save(alltweets, file = "alltweets.Rda")
