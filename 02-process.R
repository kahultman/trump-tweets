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



library(tidytext)
library(caret)

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- alltweets2 %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) 

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

unique(nrc$sentiment)

nrc_dummy <- dummyVars(~sentiment, data = nrc, sep = ".", levelsOnly = TRUE)
nrc_dummy <- as.data.frame(predict(nrc_dummy, newdata = nrc))
