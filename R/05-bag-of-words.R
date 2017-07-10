# Bag of words

# Create Bag of Words
library(tm)
library(SnowballC)

# tweet_corpus <- VCorpus(VectorSource(train_tweet$text))
# 
# tweet_corpus_clean <- tm_map(tweet_corpus, content_transformer(tolower)) %>% 
#   tm_map(removeNumbers) %>% 
#   tm_map(removeWords, stopwords()) %>% 
#   tm_map(removePunctuation) %>% 
#   tm_map(stemDocument) %>% 
#   tm_map(stripWhitespace)
# 
# tweet_dtm <- DocumentTermMatrix(tweet_corpus_clean)
# # filter out infrequent words
# tweet_freq_words <- findFreqTerms(tweet_dtm, lowfreq = 5)
# tweet_dtm <- tweet_dtm[, tweet_freq_words]


