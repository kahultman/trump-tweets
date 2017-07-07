# Modeling


# Naive Bayes without bag of words

# Function to convert numerical to categorical
convert_counts <- function(x){
  x <- as.factor(ifelse(x > 0, "Yes", "No"))
}

tweet_train <- tweet_train %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment")) 
  
tweet_test <- tweet_test %>% 
  mutate_each(funs(convert_counts), starts_with("sentiment"))
  
  
tweet_train_variables <- tweet_train %>% select(-id, -text)
  
#tweet_train <- apply(tweet_train, MARGIN = 2, convert_counts)
#tweet_test <- apply(tweet_test, MARGIN = 2, convert_counts)


### Apply Naive Bayes Model


library(e1071)
library(caret)
tweet_nb <- naiveBayes(trump~., data = tweet_train_variables, laplace = 1)
saveRDS(tweet_nb, file = "tweet_nb.Rds")

tweet_test$nb_pred <- predict(tweet_nb, newdata = tweet_test)

nb_cm <- confusionMatrix(tweet_test$nb_pred,tweet_test1$trump, positive = "trump")
nb_cm

tweet_test %>% filter(trump == "trump", nb_pred == "not trump") %>% select(text)

# Random Forest
library(randomForest)
tweet_rf <- randomForest(trump~., data = tweet_train_variables)

tweet_test$rf_pred <- predict(tweet_rf, newdata = tweet_test)
rf_cm <- confusionMatrix(tweet_test$rf_pred, tweet_test$trump, positive = "trump")
rf_cm

tweet_test %>% filter(trump == "trump", rf_pred == "not trump") %>% select(text)

tweet_test %>% filter(trump == "not trump", rf_pred == "trump", nb_pred == "trump") %>% select(text)
