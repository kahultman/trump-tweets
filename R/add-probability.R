library(tidyverse)
load("../data/trump_tweets.Rdata")
tweet_nb <- readRDS("../data/tweet_nb.Rds")

posterior <- predict(tweet_nb, newdata = trump_tweets[,1:15], type = "raw")

trump_tweets$probability <- posterior[,2]

ggplot(trump_tweets, aes(x=probability)) + geom_dotplot(aes(color=prediction))

save(trump_tweets, file = "../data/trump_tweets.RData")
