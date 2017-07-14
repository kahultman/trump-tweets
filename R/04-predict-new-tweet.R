# packages
library(stringr)
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tidyverse))
library(purrr)
library(purrrlyr)
suppressPackageStartupMessages(library(twitteR))
library(tidytext)
library(e1071)

# Get response function, if reply is necessary
get_response <- function() {
  response_list <- c("Yep, this is me.", 
                   "Can you believe I'm president?", 
                   "Hold my beer...", 
                   "Big league", 
                   "SAD!", 
                   "It's really me, I think...",
                   "Me again",
                   "I'm Donald Trump, and I approved this message:",
                   "Not my staff, I swear")

  randomnum <- sample(1:6, 1)
  response <- paste(response_list[randomnum], "@realDonaldTrump", sep = " ")
}

# Function to convert numerical to categorical
convert_counts <- function(x){
  x <- as.factor(ifelse(x > 0, "Yes", "No"))
}


# Get latest tweet

source("twitterauth.R")
setup_twitter_oauth(twitter_consumer_key,
                    twitter_consumer_secret,
                    twitter_access_token,
                    twitter_access_token_secret)

new_trump_tweet <- userTimeline("realDonaldTrump", n = 10)
new_trump_tweet <- tbl_df(map_df(new_trump_tweet, as.data.frame))

# Check if there are new tweets

print(Sys.time())
print("Checking twitter feed... ")

load("../data/trump_tweets.Rdata")
new_trump_tweet <- new_trump_tweet %>% filter(!id %in% trump_tweets$id)

if(nrow(new_trump_tweet)>0) {

  # Create features
  new_trump_tweet <- new_trump_tweet %>% 
    mutate(quote = ifelse(str_detect(text, '^"'), TRUE, FALSE)) %>% 
    mutate(text = ifelse(str_detect(text, '^"'), "", text)) %>% 
    mutate(picture = ifelse(str_detect(text, "t.co"), TRUE, FALSE)) %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
    mutate(hashtag = ifelse(str_detect(text, "#"), TRUE, FALSE)) %>% 
    mutate(date.time = ymd_hms(created, tz = "EST")) %>% 
    mutate(dow = wday(date.time, label = TRUE)) %>% 
    mutate(tod = hour(with_tz(created, tzone = "EST")))
  
  # Get sentiment
  load("../data/nrc_dummy.Rdata")
  new_trump_sentiment <- new_trump_tweet %>%
    filter(!quote) %>% 
    unnest_tokens(output = word, input = text, token = "words") %>% 
    inner_join(nrc_dummy) %>% 
    group_by(id) %>% 
    summarise_at(vars(starts_with("sentiment")), max) %>% 
    right_join(new_trump_tweet, by = "id") 
  
  # Clean up data
  new_trump_sentiment[is.na(new_trump_sentiment)] <- 0
  
  new_trump_sentiment <- new_trump_sentiment %>% 
    mutate(tod = factor(tod, c(1:23))) %>%  
    mutate_at(vars(starts_with("sentiment")), convert_counts) %>% 
    select(quote, picture, hashtag, dow, tod, starts_with("sentiment"), id, text)
  
  # load Naive Bayes model and make prediction
  tweet_nb <- readRDS("../data/tweet_nb.Rds")
  new_trump_sentiment$prediction <- predict(tweet_nb, newdata = new_trump_sentiment[,1:15])

  # Reply to tweets if predicted to be trump
  # Really hate using a loop, but not sure how to execute the function otherwise
  replytweets <- new_trump_sentiment %>% 
      filter(prediction == "trump") 
  
  print("Breakdown of new tweet predictions")
  print(table(new_trump_sentiment$prediction))
  
  if(nrow(replytweets) > 0){
    for(n in 1:nrow(replytweets)){
      updateStatus(text = get_response(), inReplyTo = replytweets$id[n])
    }
  }
  
  #save new tweets to file
  trump_tweets <- rbind(trump_tweets, new_trump_sentiment)  
  save(trump_tweets, file = "../data/trump_tweets.RData")

} else {
  print("There were no new tweets")
}

rm(list = ls(all=TRUE))
