#always install this library
library("tidyverse")

#machine learning package
library("xgboost")
train <- read_csv("train_V2.csv") # reads in files
#creates sample - set.seed means same random sample used every time code runs
train <- na.omit(train)
set.seed(1234)
data_sample <- sample_n(train, 4440000)


#method to deterine whether player is in top half or not boolean function
topTierPlayers <- function(x){
  return(x > 0.5)
}

#turns all unique match types bar 1 into a matrix with the same number of rows as train_sample then turns it into a data frame
non_numeric <- model.matrix(~matchType-1, data_sample)
non_numeric <- as.data.frame(non_numeric)

#adds top50 column to train_sample, removes winPlacePerc + matchType + Id columns, adds matchType matrix to data_sample
data_sample <- data_sample %>%
  
  mutate(top50 = topTierPlayers(winPlacePerc)) %>%
  select(-winPlacePerc) %>%
  cbind(non_numeric)  %>%
  select(-matchType) %>%
  select(-ends_with("Id")) 

#put top50 column into a separate vector
labels <- data_sample$top50
data_sample <- select(data_sample, -top50)

#split 70% of data to training data and other 30% into testing data
NumberOfSamples <- round(nrow(data_sample) * 0.7)
training_data <- data_sample[1:NumberOfSamples,]
training_label <- labels[1:NumberOfSamples]
test_data <- data_sample[-(1:NumberOfSamples),]
test_label <- labels[-(1:NumberOfSamples)]

training_data <- data.matrix(training_data)
test_data <- data.matrix(test_data)

dtrain <- xgb.DMatrix(data = training_data, label = training_label)
dtest <- xgb.DMatrix(data = test_data, label = test_label)

train_model <- xgboost(data = dtrain, nrounds = 20, objective = "binary:logistic")

pred <- predict(train_model, dtest)
pred

results <- data.frame(prediction = pred >= 0.5, test_label)
results

err <- mean(as.numeric(pred> 0.5) != test_label)
err