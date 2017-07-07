# Process data

load("alltweets.Rda")


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

#reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- alltweets2 %>%
  filter(!quote) 

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)


nrc_dummy <- dummyVars(~sentiment, data = nrc, sep = ".", levelsOnly = TRUE)
nrc_dummy <- as.data.frame(predict(nrc_dummy, newdata = nrc))
nrc_dummy$word <- nrc$word 

save(nrc_dummy, file = "nrc_dummy.Rdata")

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
train_tweet <- alltweets2 %>% 
  filter(date.time < "2017-03-01") %>% 
  mutate(trump = as.factor(if_else(source == "Android", true = "trump", false = "not trump"))) %>% 
  select(-source, -created, -date.time)

# Create Bag of Words
library(tm)
library(SnowballC)

tweet_corpus <- VCorpus(VectorSource(train_tweet$text))

tweet_corpus_clean <- tm_map(tweet_corpus, content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords()) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

tweet_dtm <- DocumentTermMatrix(tweet_corpus_clean)
# filter out infrequent words
tweet_freq_words <- findFreqTerms(tweet_dtm, lowfreq = 5)
tweet_dtm <- tweet_dtm[, tweet_freq_words]

# Split into training and test sets

set.seed(145)
in_training <- createDataPartition(train_tweet$trump, times = 1, p = 0.8, list = FALSE)
tweet_test <- train_tweet[-in_training,] 
tweet_train <- train_tweet[in_training,] 
tweet_dtm_test <- tweet_dtm[-in_training,]
tweet_dtm_train <- tweet_dtm[in_training,]
