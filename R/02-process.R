# Process data

load("../data/alltweets.Rda")

library(stringr)
library(lubridate)
library(tidyverse)

alltweets2 <- alltweets %>% 
  mutate(quote = ifelse(str_detect(text, '^"'), TRUE, FALSE)) %>% 
  mutate(text = ifelse(str_detect(text, '^"'), "", text)) %>% 
  mutate(picture = ifelse(str_detect(text, "t.co"), TRUE, FALSE)) %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
  mutate(hashtag = ifelse(str_detect(text, "#"), TRUE, FALSE)) %>% 
  mutate(date.time = ymd_hms(created, tz = "EST")) %>% 
  mutate(dow = wday(date.time, label = TRUE)) %>% 
  mutate(tod = hour(with_tz(created, tzone = "EST")))

# Sentiment data
library(tidytext)
library(caret)

tweet_words <- alltweets2 %>%
  filter(!quote) 

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

nrc_dummy <- dummyVars(~sentiment, data = nrc, sep = ".", levelsOnly = TRUE)
nrc_dummy <- as.data.frame(predict(nrc_dummy, newdata = nrc))
nrc_dummy$word <- nrc$word 

save(nrc_dummy, file = "../data/nrc_dummy.Rdata")

# Tidy the tweets - one word per line
tidy_tweet <- tweet_words %>% unnest_tokens(output = word, input = text, token = "words")
tidy_tweet <- inner_join(tidy_tweet, nrc_dummy)

# Aggregate sentiment scores per tweet
tweet_sentiment <- tidy_tweet %>% group_by(id) %>% 
  summarise_each(funs(max),starts_with("sentiment"))

# combine sentiment scores with full data set
alltweets2 <- left_join(alltweets2, tweet_sentiment, by = "id") 
alltweets2[is.na(alltweets2)] <- 0

# Break into train/test and new data
# Exclude tweets after March 8, 2017
# shared <- alltweets2 %>% 
#   filter(date.time < "2017-03-08") %>% 
#   mutate(trump = as.factor(if_else(source == "Android", true = "trump", false = "not trump"))) %>% 
#   select(-source, -created)
# 
# write_csv(shared, "trump-tweets-processed.csv")

future <- alltweets2 %>% 
  filter(date.time > "2017-03-08") %>% 
  select(-source, -created)

# Save Files

save(future, file = "../data/future.Rdata")
save(alltweets2, file = "../data/alltweets-processed.Rdata")

