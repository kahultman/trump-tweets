# Modeling


# Naive Bayes without bag of words

# Function to convert numerical to categorical
convert_counts <- function(x){
  x <- ifelse(x > 0, "Yes", "No")
}

tweet_train <- apply(tweet_train, MARGIN = 2, convert_counts)
tweet_test <- apply(tweet_test, MARGIN = 2, convert_counts)


### Apply Naive Bayes Model

library(e1071)
library(caret)
tweet_nb <- naiveBayes(trump~., data = tweet_train, laplace = 0)

nb_pred <- predict(tweet_nb, newdata = tweet_test)

nb_cm <- confusionMatrix(nb_pred,tweet_test$trump, positive = "trump")
nb_cm


# Random Forest
library(randomForest)
tweet_rf <- randomForest(trump~., data = tweet_train)

rf_pred <- predict(tweet_rf, newdata = tweet_test)
rf_cm <- confusionMatrix(rf_pred, tweet_test$trump, positive = "trump")
rf_cm