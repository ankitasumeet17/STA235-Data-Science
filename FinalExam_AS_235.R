# Ankita Sumeet - Sager 235 
rm(list=ls())
con <- url("https://github.com/eyucel/STA235/raw/main/data.RData")
load(con)
rm(con)

library(tidyverse)
library(modelr) # for common evaluation metrics
library(rsample)  # for creating train/test splits
library(mosaic)

options(scipen = 999)

# q1
model1 = lm(test_scores ~ ratio + income + ratio:income , data = schools)
summary(model1)

# q2
model2 = lm(test_scores ~ ratio + english , data = schools)
rsquared(model2)

# q3
model3 = lm(test_scores ~ ratio + expenditures , data = schools)
summary(model3)
(exp(0.002344) - 1) * 100

# q4 (filter)
model4.1 = lm(test_scores ~ ratio + grade_span , data = schools)
summary(model4.1)

new = data.frame(ratio = , grade_span = "KK-06")
predict(model4.1, newdata = new, interval = 'predict')

model4.2 = lm(test_scores ~ ratio + grade_span , data = schools)
summary(model4.2)

neww = data.frame(ratio = , grade_span = "KK-08")
predict(model4.2, newdata = neww, interval = 'predict')

# q5
options(scipen = 999)   # avoid scientific notation in output
salesmodel1 <- lm(sales ~ month, data=salesdata)
summary(salesmodel1)
plot(salesmodel1)

salesmodel2 <- lm(sales ~ month + I(month^2), data=salesdata)
summary(salesmodel2)
acf(salesdata$sales)
plot(salesmodel2)

# q9 (filter)
multreg1 = lm(TotalKg ~ Sex + Age + BodyweightKg , data = Lift)
summary(multreg1)

new1 = data.frame(Sex = "M", Age = 38.0, BodyweightKg = 65.0)
predict(multreg1, newdata = new1, interval = 'predict')

# q10 
multlin1 <- glm(TotalKg ~ Sex + Age + BodyweightKg , data = Lift)
plot(multlin1)

# q11 (filter)
multlin2 = lm(log(TotalKg) ~ Sex + Age + log(BodyweightKg) + Sex:log(BodyweightKg) , data = Lift)
summary(multlin2)
(exp(0.471097) - 1) * 100

# q12 (filter)
multlin3 = lm(log(TotalKg) ~ Sex + Age + log(BodyweightKg) + Sex:log(BodyweightKg) , data = Lift)
summary(multlin3)

new3 = data.frame(Sex = "M", Age = 20.0, BodyweightKg = 65.0)
predict(multlin3, newdata = new3, interval = 'predict')  

# q13
options(scipen = 999)                    # avoid scientific notation in output
tipsmodel <- lm(tips ~ day + mon + tue + thu + fri + sat + sun, data=restaurantdata)
summary(tipsmodel)
acf(residuals(tipsmodel))

predict(tipsmodel, list(day = 0 , mon = 0 , tue = 0 , thu = 0 , fri = 1 , sat = 0 , sun = 0) , interval="prediction") 

# q14
residuals(tipsmodel)[-1]

# q17
exp(0.06023)-1

# q18
0.980 / 0.32723

# q20
logisticmodel = lm(LowBwt ~ . , data = babywt)
summary(logisticmodel)

# q21
AIC(logisticmodel) - AIC(step(logisticmodel))

# q22
Train_index=1:500
Train_data = babywt[Train_index,]
Test_data = babywt[-Train_index,]

fullmodel =  glm(LowBwt~.,family=binomial,data=Train_data)

Test_data$prediction <- ifelse(predict(fullmodel,Test_data,type="response") >0.5, 'low', 'normal')
Test_data$actual <- ifelse(Test_data$LowBwt==0, 'normal', 'low')

table(Test_data$prediction, Test_data$actual)

(12 + 570) / (12 + 570 + 3 + 89)

# q33
  # Model A
  (52 + 20) / (52 + 20 + 15 + 13) # accuracy 
  (15 + 13) / (52 + 20 + 15 + 13) # accuracy 
  (13 + 20 / 15 + 20) # precision
  (52 + 15) / (52 + 20 + 15 + 13) # recall
  (15) / (52 + 20 + 15 + 13) # false neg

  # Model B
  (42 + 30) / (42 + 30 + 5 + 23) # accuracy 
  (5 + 23) / (52 + 20 + 15 + 13) # accuracy 
  (23 + 30 / 5 + 30) # precision
  (42 + 5) / (52 + 20 + 15 + 13) # recall
  (5) / (42 + 30 + 5 + 23) # false neg
