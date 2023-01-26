library(tidyverse)
library(mosaic)
library(ggplot2)

# q1
lm1 = lm(Sales ~ Price, data = OJ)
plot(lm1)
hist(lm1)

#q2
loglm = lm(log(Sales) ~ log(Price), data=OJ)

employment <- employment %>% mutate(treat= ifelse(year_mw != 0, 1, 0))

employment_pre <- employment %>% filter(year < year_mw | treat == 0)

employment_balance <- employment_pre %>% select(lemp, lpop, treat)

datasummary_balance(~ treat, data = employment_balance, fmt = 4, dinm_statistic = "p.value")

plot(loglm)
summary(loglm)

#q3
p = list(Price=seq(1.5,2, .1))
predict(loglm, p, interval='pred')

ggplot() + 
  geom_point(data= OJ, aes(x=Price, y=Sales), size=2) +
  geom_line(aes(x=p$Price, y=exp(pred[,1])), size=2, colour='blue') + 
  geom_ribbon(aes(x =p$Price, ymin=exp(pred[,2]), ymax=exp(pred[,3])), alpha=.2)


#q4
lm2 = lm(log(hours) ~ log(wage), data=nlsw88)
plot(lm2)

ggplot(nlsw88) + 
  geom_histogram(aes(x=wage))
                 
#q5/6
lm3 = lm(log(wage)~tenure + grade +ttl_exp, data= nlsw88)
plot(lm3)
lm4 = lm(wage~tenure + grade + ttl_exp, data = nlsw88)
plot(lm4)

#q7
view(electricity_usage)
lm5 = lm(MonthlyUsage~Size, data = electricity_usage)
plot(lm5)
summary(lm5)

# q8
lm6 = lm(MonthlyUsage~Size + l(Size^2), data = electricity_usage)
summary(lm6)


#q9
new = data.frame(Size=4000)
predict(lm6 , newdata = new , interval ='predict')

#q10
lm2 = lm(log(hours) ~ log(wage), data=nlsw88)
plot(lm2)

ggplot(nlsw88) + 
  geom_histogram(aes(x=wage))

#q11
lm3 = lm(log(wage)~tenure + grade +ttl_exp, data= nlsw88)
plot(lm3)
lm4 = lm(wage~tenure + grade + ttl_exp, data = nlsw88)
plot(lm4)




