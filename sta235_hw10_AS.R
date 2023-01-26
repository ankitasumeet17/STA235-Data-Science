library(tidyverse)
library(modelr) # for common evaluation metrics
library(rsample)  # for creating train/test splits
library(mosaic)

# get rid of scientific notation 
options(scipen = 999)

# This "set.seed" function fixes the random-number seed.
# This ensures we will get the same thing if we run a Monte Carlo simulation twice.
set.seed(1720101425)

# question 4
xtabs(~resumes$call + resumes$ethnicity) 

# question 5
call_boot = do(1000) * diffprop(call ~ ethnicity, data = resample(resumes))

ggplot(call_boot) +
  geom_histogram(aes(x=diffprop), color = 'white', fill = 'navyblue')

confint(call_boot) %>%
  select(-level, -method) %>%
  mutate_if(is.numeric, round, digits=3)


# question 6
resumes = resumes %>%
  mutate(ethnicity = relevel(as.factor(ethnicity), ref = 'white'))

resumes = resumes %>%  mutate(callback=(call=='call')*1)

view(resumes)

call_back1 = glm(callback ~ ethnicity, family = 'binomial', data = resumes)
summary(call_back1)
1-exp(-0.43818) #0.3547904

# question 7
resumes = resumes %>%  
  mutate(block = interaction(city, quality))

xtabs(~resumes$call + resumes$block)


# question 8
logmdl = glm(callback ~ ethnicity + block, family = 'binomial', data = resumes)
AIC(call_back1)
AIC(call_back2)
summary(call_back2)
-0.4398 + -0.5844
1-exp(-1.0242)


# question 9
exp(confint(logmdl))

# question 10
predict(logmdl, list(ethnicity='black', block='boston.high'), type = 'response')
predict(logmdl, list(ethnicity='white', block='boston.high'), type = 'response')
predict(logmdl, list(ethnicity='black', block='chicago.low'), type = 'response')
predict(logmdl, list(ethnicity='white', block='chicago.low'), type = 'response')

# question 11
1/predict(logmdl, list(ethnicity='black', block='chicago.low'), type = 'response')
1/predict(logmdl, list(ethnicity='white', block='chicago.low'), type = 'response')
