library(tidyverse)
library(modelr) # for common evaluation metrics
library(rsample)  # for creating train/test splits
library(mosaic)

# get rid of scientific notation 
options(scipen = 999)

# This "set.seed" function fixes the random-number seed.
# This ensures we will get the same thing if we run a Monte Carlo simulation twice.
set.seed(1720101425)

# question 1
lm = lm(agecell ~ all + mva + internal, data = mlda)
summary(lm)

ggplot(mlda) + 
  geom_point(aes(x=agecell, y=mva))

# question 3

# question 4

# question 5

# question 6

# question 7

# question 8
21 - 0.002739726 # = 20.99726
D1 = 50.46505

mlda = mlda %>%
  mutate(dist = agecell-c, legal = ifelse(agecell>21,1,0))

lm8 = lm(mva ~ dist*legal, data = mlda)
summary(lm8)

predict(lm8, list(dist - 0.002739726, legal = 4.5340), interval = "prediction")

21 + 0.002739726 # = 21.00274
D1 = 54.9959

mlda = mlda %>%
  mutate(dist = agecell-c, legal = ifelse(agecell<21,1,0))

lm8 = lm(mva ~ dist*legal, data = mlda)
summary(lm8)

predict(lm8, list(dist - 0.002739726, legal = -4.5340), interval = "prediction")

# question 10














