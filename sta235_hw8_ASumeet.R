library(tidyverse)
library(mosaic)
options(scipen = 999)
library(lmtest)

#1
model1 = glm(Buy ~ Income, data=magazine, family='binomial')
predict(model1, data.frame(Income=50), type="response") %>% round(2)

#2
predict(model1, data.frame(Income=40), type="response") #0.03330948
predict(model1, data.frame(Income=50), type="response") #0.1330785
predict(model1, data.frame(Income=60), type="response") #0.4061313

#3, 4
summary(model1)
(exp(0.14940)-1)*100

# 8.5 - 50% as the cutoff probability - P(success) >= 0.5)
#     - what percentage of web site visitors does the model correctly predict purchase behavior? 
predicted <- (predict(model1, type="response") >= 0.5)
actual <- (magazine$Buy == 1)
table(predicted, actual)

((525 + 99) / (525 + 26 + 23 + 99)) * 100

#6
modelnull = glm(Buy ~ 1, data=magazine, family='binomial')
summary(modelnull)

magazine = magazine %>% 
  mutate(predicted = ifelse(predict(modelnull, type="response") >=0.5, "success", "failure"),
         actual = ifelse(Buy==1, "success", "failure"))
xtabs(~predicted + actual, data=magazine)

#7
magazine = magazine %>% 
  mutate(predicted = ifelse(predict(model1, type="response") >=0.5, "success", "failure"),
         actual = ifelse(Buy==1, "success", "failure"))
xtabs(~predicted + actual, data=magazine)

#8
model2 = glm(Buy ~ Income + Female + Own + White + PrevParentMag, 
             data=magazine, family='binomial')
summary(model2)
exp(1.56537)

#9
lrtest(model1, model2)

#10
magazine = magazine %>% 
  mutate(predicted = ifelse(predict(model2, type="response") >=0.5, "success", "failure"),
         actual = ifelse(Buy==1, "success", "failure"))
xtabs(~predicted + actual, data=magazine)
(527+106)/(527+106+19+21)*100

#11
summary(model2)
