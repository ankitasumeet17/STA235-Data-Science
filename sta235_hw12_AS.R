library(tidyverse)
library(modelr) # for common evaluation metrics
library(rsample)  # for creating train/test splits
library(mosaic)

# get rid of scientific notation 
options(scipen = 999)

# This "set.seed" function fixes the random-number seed.
# This ensures we will get the same thing if we run a Monte Carlo simulation twice.
set.seed(1720101425)

# question 3
library(caret)
head(wisc.trn)
wisc.trn <- wisc[1:469,]
wisc.tst<- wisc[-(1:469),]

set.seed(123)
wisc.knn <- train(
  class ~., data = wisc.trn, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  # preProcess = c("center","scale"),
  tuneGrid = expand.grid(k=seq(1,101,by=2))
)
wisc.knn$bestTune$k

# question 4 
predicted = predict(wisc.knn, wisc.tst)
actual = wisc.tst$class
table(predicted, actual)

# question 5  
wisc.trn <- wisc[1:469,]
wisc.tst<- wisc[-(1:469),]

set.seed(123)
wisc.knn <- train(
  class ~., data = wisc.trn, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k=seq(1,101,by=2))
)
wisc.knn$bestTune$k

# question 6  0.94
predicted = predict(wisc.knn, wisc.tst)
actual = wisc.tst$class
table(predicted, actual)
(72+22)/(72+1+5+22)

# question 7
22/23

# question 8
22/27

# question 11
library(tidyverse)
set.seed(123)
wisc1.trn <- wisc.trn %>% mutate(class = ifelse(class=='M',1,0))
wisc1.tst <- wisc.tst %>% mutate(class = ifelse(class=='M',1,0))
wisc1.log <- glm(class~., data = wisc1.trn, family = "binomial")

predicted <- predict(wisc1.log, wisc1.tst, type = "response")
actual <- wisc1.tst$class
table(predicted, actual)

prediction <- ifelse(predicted >= 0.5, "M", "B")
table(prediction, actual)
# Accuracy = 0.91
(70+21)/(70+2+21+7)
# Recall = 0.913
21/23
# Precision = 0.75
21/28

prediction <- ifelse(predicted >= 0.9, "M", "B")
table(prediction, actual)
# Accuracy = 0.98
(77+21)/(77+2+21)
# Recall = 0.913
21/23
# Precision = 1
21/21
prediction <- ifelse(predicted >= 0.95, "M", "B")
table(prediction, actual)
# Accuracy = 0.96
(77+19)/(77+4+19)
# Recall = 0.826
(19/23)
# Precision = 1
19/19
