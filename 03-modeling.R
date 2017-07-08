# Modeling


# load packages and data
library(tidyverse)
library(e1071)
library(caret)
library(randomForest)

load("tweet_train.Rdata")
load("tweet_test.Rdata")

# Function to convert numerical to categorical
convert_counts <- function(x){
  x <- as.factor(ifelse(x > 0, "Yes", "No"))
}

tweet_train <- tweet_train %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment")) %>% 
  mutate(tod = as.factor(tod))
  
tweet_test <- tweet_test %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment")) %>% 
  mutate(tod = as.factor(tod))
  
  
tweet_train_variables <- tweet_train %>% select(-id, -text)
  
#tweet_train <- apply(tweet_train, MARGIN = 2, convert_counts)
#tweet_test <- apply(tweet_test, MARGIN = 2, convert_counts)


### Apply Naive Bayes Model

tweet_nb <- naiveBayes(trump~., data = tweet_train_variables, laplace = 1)
saveRDS(tweet_nb, file = "tweet_nb.Rds")
tweet_nb

tweet_test$nb_pred <- predict(tweet_nb, newdata = tweet_test)

nb_cm <- confusionMatrix(tweet_test$nb_pred,tweet_test$trump, positive = "trump")
nb_cm

tweet_test %>% 
  filter(trump == "trump", nb_pred == "not trump") %>% 
  View()

### Apply Naive Bayes to future dataset

load("future.Rdata")
future <- future %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment")) %>% 
  mutate(tod = as.factor(tod))

future$nb_pred <- NULL
future$nb_pred <- predict(tweet_nb, newdata = future, type = "class")
table(future$nb_pred)
write_csv(future, "future-predicted.csv")

# Random Forest

tweet_rf <- randomForest(trump~., data = tweet_train_variables)

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
