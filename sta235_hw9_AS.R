library(tidyverse)
library(mosaic)
options(scipen = 999)
library(lmtest)

# q1 
lm1 = lm(bwt ~ gestation + parity + age + height + weight + smoke, data = babyweight)
summary(lm1)

# q2
summary(lm(bwt~ parity+age+height+weight+smoke,data=babyweight))$adj.r.squared # 0.1061082
summary(lm(bwt~gestation+age+height+weight+smoke,data=babyweight))$adj.r.squared # 0.2492304
summary(lm(bwt~gestation+parity+height+weight+smoke,data=babyweight))$adj.r.squared # 0.25477
summary(lm(bwt~gestation+parity+age+weight+smoke,data=babyweight))$adj.r.squared # 0.2345441
summary(lm(bwt~gestation+parity+age+height+smoke,data=babyweight))$adj.r.squared # 0.2522547
summary(lm(bwt~gestation+parity+age+height+weight,data=babyweight))$adj.r.squared # 0.2052414

# q3
backMod1 = step(lm1)
AIC(backMod1) # 9823.503
AIC(lm1) # 9825.492
AIC(backMod1)-AIC(lm1) # -1.989058

# q5
bNull =lm(bwt~1, data=babyweight)
summary(lm(bwt~weight, data=babyweight))$adj.r.squared # 0.02347957

# q6
summary(lm(bwt~gestation, data=babyweight))$adj.r.squared # 0.1653796
summary(lm(bwt~parity, data=babyweight))$adj.r.squared # 0.00107633
summary(lm(bwt~age, data=babyweight))$adj.r.squared # -0.0001245436
summary(lm(bwt~height, data=babyweight))$adj.r.squared # 0.04067756
summary(lm(bwt~weight, data=babyweight))$adj.r.squared # 0.02347957
summary(lm(bwt~smoke, data=babyweight))$adj.r.squared # 0.06010873

# q7
summary(lm(bwt~gestation+parity, data=babyweight))$adj.r.squared # 0.1706273
summary(lm(bwt~gestation+age, data=babyweight))$adj.r.squared # 0.1670549
summary(lm(bwt~gestation+height, data=babyweight))$adj.r.squared # 0.1954919
summary(lm(bwt~gestation+weight, data=babyweight))$adj.r.squared # 0.1861141
summary(lm(bwt~gestation+smoke, data=babyweight))$adj.r.squared # 0.2143214

# q8
2^6

# q9
library(caret)
set.seed(123)
Train_index = 1:300
Train_data = babyweight[Train_index,] # View(Train_data)
Test_data = babyweight[-Train_index,] # View(Test_data)

bwtComplex = lm(bwt~gestation+parity+age+height+weight+smoke, data = Train_data)
rmse(bwtComplex, Test_data) # 16.33143

# q10
backMod2 = step(babyweight1_complex)
bwtComplex2 = lm(bwt~gestation+parity+height+smoke,data=Train_data)
rmse(bwtComplex2, Test_data) # 16.27247













