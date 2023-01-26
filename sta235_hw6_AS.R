library(tidyverse)
library(mosaic)
library(ggthemes)
theme_set(theme_classic()) 
options (scipen = 999)

# Q1
lm1 = lm(sales ~ Qtr, data = TechSoft)
lm1
summary(lm1)
plot(lm1)

lm2 = lm(sales ~ Qtr2, data = TechSoft)
lm2
summary(lm2)
plot(lm2)


lm3 = lm(sales ~ Qtr+Qtr2, data = TechSoft)
lm3
summary(lm3)
plot(lm3)

lm4 = lm(log(sales) ~ Qtr, data = TechSoft)
lm4
summary(lm4)
plot(lm4)

# Q 2 / 3
lm5 = lm(sales ~ I(Qtr^2), data = TechSoft)
lm5
summary(lm5)

plot(lm5)

TechSoft  %>% 
  mutate(detrend = residuals(lm3)) %>%
  mutate(trend = fitted(lm3))

residuals(lm3)[100]


# Q 5 / 6
predict(lm3, newdata = data.frame(Qtr = 104, Qtr2 = I(104^2)), 
        interval = 'predict')  


# Q7 
lm_szn = lm(detrend ~ S2 + S3 + S4, data = TechSoft)

predict(lm_szn, list(S2 = 0, S3 = 0, S4 = 1), interval = 'predict')

summary(lm_szn)


# Q8
lm_szn1 = lm(detrend ~ S1 + S3 + S4, data = TechSoft)
predict(lm_szn1, list(S1 = 0, S3 = 0, S4 = 1),interval = 'prediction')

summary(lm_szn1)

# Q9
lm_comb = lm(sales ~ Qtr + Qtr2 + S2 + S3 + S4, data = TechSoft)
summary(lm_comb)

predict(lm_comb, newdata = data.frame(Qtr = 104, 
                                      Qtr2 = I(104^2), S2 = 0, S3 = 0, S4 = 1 ),
        interval = 'predict')


# Q 10
szn_model =  lm(detrend ~ S2 + S3 + S4 , data = TechSoft)
summary(szn_model) 


# Q 11 / 12
comb_decomp = lm(sales ~ Qtr + Qtr2 + S2 + S3 + S4 , data = TechSoft)
predict(comb_decomp , newdata = data.frame(Qtr = 104 , Qtr2 = 10816 , S2 = 0 , S3 = 0, S4 = 1), interval = 'predict') 
# fit - 193.8567 , upper - 204.0354
