# Modeling


# load packages and data
library(tidyverse)
library(e1071)
library(caret)
library(randomForest)

load("../data/alltweets-processed.Rdata")

# Function to convert numerical to categorical
convert_counts <- function(x){
  x <- as.factor(ifelse(x > 0, "Yes", "No"))
}

alltweets2 <- alltweets2 %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment")) %>% 
  mutate(tod = as.factor(tod))
  

# Split into training and test sets
train_tweet <- alltweets2 %>% 
  filter(date.time < "2017-03-08") %>% 
  mutate(trump = as.factor(if_else(source == "Android", true = "trump", false = "not trump"))) %>% 
  select(-source, -created, -date.time)

set.seed(45)
in_training <- createDataPartition(train_tweet$trump, times = 1, p = 0.8, list = FALSE)
tweet_test <- train_tweet[-in_training,] 
tweet_train <- train_tweet[in_training,] 


### Apply Naive Bayes Model

tweet_nb <- naiveBayes(trump~., data = tweet_train[,3:18], laplace = 1)
saveRDS(tweet_nb, file = "../data/tweet_nb.Rds")
tweet_nb

tweet_test$nb_pred <- predict(tweet_nb, newdata = tweet_test)

nb_cm <- confusionMatrix(tweet_test$nb_pred,tweet_test$trump, positive = "trump")
nb_cm

tweet_test %>% 
  filter(trump == "trump", nb_pred == "not trump") %>% 
  View()

### Apply Naive Bayes to future dataset

load("../data/future.Rdata")
future <- future %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment")) %>% 
  mutate(tod = as.factor(tod))

future$nb_pred <- predict(tweet_nb, newdata = future, type = "class")
table(future$nb_pred)
write_csv(future, "future-predicted.csv")

# Random Forest

tweet_rf <- randomForest(trump~., data = tweet_train[,3:18])

tweet_test$rf_pred <- predict(tweet_rf, newdata = tweet_test)

rf_cm <- confusionMatrix(tweet_test$rf_pred, tweet_test$trump, positive = "trump")
rf_cm

tweet_test %>% filter(trump == "trump", rf_pred == "not trump") %>% select(text)

tweet_test %>% filter(trump == "not trump", rf_pred == "trump", nb_pred == "trump") %>% select(text)

# Require both models to predict Trump to call it trump

tweet_test <- tweet_test %>% 
  mutate(ensemble = ifelse(nb_pred == "trump" & rf_pred == "trump", "trump", "not trump"))
ensemble_cm <- confusionMatrix(tweet_test$ensemble, tweet_test$trump, positive = "trump")
ensemble_cm


save(tweet_test, file = "../data/tweet_test.Rdata")
save(tweet_train, file = "../data/tweet_train.Rdata")
