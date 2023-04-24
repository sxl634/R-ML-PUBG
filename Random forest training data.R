library("randomForest")
library("tidyverse")
train <- read_csv("train_V2.csv") # reads in files
#creates sample - set.seed means same random sample used every time code runs
train <- na.omit(train)
set.seed(1234)
data_sample <- sample_n(train, 1000000)

topTierPlayers <- function(x){
  return(x >= 0.5)
}

matchTypes <- model.matrix(~matchType-1, data_sample)
matchTypes <- as.data.frame(matchTypes)

data_sample <- data_sample %>%
  mutate(top50 = topTierPlayers(winPlacePerc)) %>%
  select(-winPlacePerc) %>%
  cbind(matchTypes) %>%
  select(-matchType) %>%
  select(-(ends_with("Id")))
names(data_sample) <- make.names(names(data_sample))

data_sample$top50 <- as.character(data_sample$top50)
data_sample$top50 <- as.factor(data_sample$top50)

split <- round(nrow(data_sample) * 0.7)
training_data <- data_sample[1:split,]
test_data <- data_sample[-(1:split),]

model <- randomForest(top50 ~ ., data = training_data, nodesize = 20, ntree = 20)
model
predict <- predict(model, newdata = test_data)
predict
table <- table(predict, test_data$top50)
test_matrix <- as.matrix(table)
model_error <- (test_matrix[1,1] + test_matrix[2,2]) / nrow(test_data)
model_error
