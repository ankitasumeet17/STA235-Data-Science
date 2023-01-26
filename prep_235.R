library(tidyverse)
library(modelr) # for common evaluation metrics
library(rsample)  # for creating train/test splits
library(mosaic)

# get rid of scientific notation 
# options(scipen = 999)

# This "set.seed" function fixes the random-number seed.
# This ensures we will get the same thing if we run a Monte Carlo simulation twice.
set.seed(1720101425)


# forecast of the change in price 
mean(oil$change)

# variation is r squared 
rsquared(new)

# upper limit 95 int
ar1 <- lm(price ~ lagprice, data=oil)
predict(ar1, list(lagprice=46.670), interval="prediction")

# additive decomposition model
predict(model, list(quarter=81, Q1=1, Q2=0, Q3=0), interval="prediction") 

# value of detrended and deseasonalized sales for quarter number 80
residuals(model)[80]

# 8.5 - 50% as the cutoff probability - P(success) >= 0.5)
#     - what percentage of web site visitors does the model correctly predict purchase behavior? 
model1 <- glm(Buy ~ Income, data = magazine, family = binomial)
predicted <- (predict(model1, type="response") >= 0.5)
actual <- (magazine$Buy == 1)
table(predicted, actual)

((525 + 99) / (525 + 26 + 23 + 99)) * 100

# 8.6 - compare to a "null model". give accuracy in percentage.
table(magazine$Buy) # 548 bought. 125 did not buy.
548 / (548 + 125)

# 8.8 - % of white person to buy over regular 
model2 <- glm(Buy ~ Income + Female + Own + White + PrevParentMag, data=magazine, family=binomial)
summary(model2)
3.327 / 0.000879 # z value / Pr(>|z|)

# 9.9 - Build full model with training set and validate performance on the testing set. On the test set, report the RMSE.
library(caret)
Train_index = 1:300
Train_data = babyweight[Train_index,] # View(Train_data)
Test_data = babyweight[-Train_index,] # View(Test_data)

fullmodel = lm(bwt~.,data=Train_data)
RMSE(fullmodel$fitted.values,Train_data$bwt)
TrainRMSE = sqrt(sum(fullmodel$residuals^2)/length(fullmodel$residuals))
TrainRMSE

prediction = predict(fullmodel,Test_data)
RMSE(prediction,Test_data$bwt)
TestRMSE = sqrt(sum((Test_data$bwt-prediction)^2)/length(prediction))
TestRMSE # 16.33143

# 9.10 - TRAIN w/ AIC backward selection
backward_model = step(lm(bwt~.,data=Train_data))
RMSE(backward_model$fitted.values,Train_data$bwt)

TrainRMSE_backward = sqrt(sum(backward_model$residuals^2)/length(backward_model$residuals))
TrainRMSE_backward

prediction = predict(backward_model,Test_data)
RMSE(prediction,Test_data$bwt)

TestRMSE_backward = sqrt(sum((Test_data$bwt-prediction)^2)/length(prediction))
TestRMSE_backward

# 10.4 - PROP TABLE for call back rates 
xtabs(~resumes$call) 
# 392/(392+4478) = 0.08049281 = 8%
xtabs(~resumes$call + resumes$ethnicity)
# P(call | black) = 157/(157+2278) = 0.06447639 = 6%
# P(call | white) = 235/(235+2200) = 0.09650924 = 10%

# 10.5 - CONFINT for difference in proportions, comparing proportion of Black/White résumés that received a call back
prop.test(call ~ ethnicity, data = resumes) # -0.01636705 (2nd num in 95 percent confidence interval)

# 10.6 - re-level ethnicity variable
resumes = resumes %>%
  mutate(ethnicity = relevel(as.factor(ethnicity), ref = 'white'))
#      - Fit a logistic regression model for your new dummy call variable in terms of the treatment variable ethnicity 
resumes = resumes %>%
  mutate(call_dummy = ifelse(call=='call', 1, 0)) # dummy variable for call

glm(call_dummy ~ ethnicity, data = resumes, family = 'binomial')  %>% 
  summary() # logistic regression model

# output:
#               Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)    -2.23663    0.06863 -32.590 < 0.0000000000000002 ***
# ethnicityblack -0.43818    0.10732  -4.083            0.0000445 ***
# ---
# Null deviance: 2726.9  on 4869  degrees of freedom
# Residual deviance: 2709.9  on 4868  degrees of freedom
# AIC: 2713.9
100*(exp(-0.43818)-1) # slope

# 10.7 - BLOCKING DESIGN
resumes = resumes %>%  
  mutate(block = interaction(city, quality))

xtabs(~call + block, data=resumes) %>% 
  prop.table() %>% 
  round(3) # joint probabilities

# 10.9 - logistic regression model with blocking design to calculate a 95% confidence interval 
glm_block = glm(call_dummy ~ ethnicity, data = resumes, family = 'binomial') 

100 * (confint(glm_block) %>% 
         exp() %>% 
         -1 %>% 
         round(1)) 

# 10.10 - predict 
predict(glm_block, list(ethnicity='black', block='boston.high'), type = 'response')
predict(glm_block, list(ethnicity='white', block='boston.high'), type = 'response')
predict(glm_block, list(ethnicity='black', block='chicago.low'), type = 'response')
predict(glm_block, list(ethnicity='white', block='chicago.low'), type = 'response')


# 11.3 - regression discontinuity model
mlda <- mlda %>% mutate(turned21=ifelse(agecell >= 21, 1, 0))
summary(lm(all ~ agecell + turned21, data=mlda)) 

# 11.4 - recenter 
# create yearsover21 as a recentered version of age
mlda <- mlda %>% mutate(yearsover21=age-21)
summary(lm(mva ~ yearsover21 * turned21, data=mlda))

# 11.8 - mlda model 
mva.model <- lm(mva ~ yearsover21 * turned21, data=mlda)
D1 <- predict(mva.model, list(yearsover21=-1/365, turned21=0))
D2 <- predict(mva.model, list(yearsover21=1/365, turned21=1))
D2 - D1

