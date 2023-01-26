library(mosaic)
library(tidyverse)
library(scales)

# q1
lm1 = lm(NFLX ~ lag(NFLX), data = netflix)
summary(lm1)
acf(lm1$reiduals)
plot(lm1)

# q2
summary(lm1)
(0.94236 - 1) / 0.05617

# q4
predict(lm1, newdata = data.frame(lag=476.68), interval = 'predict')

# q5 / 6
summmary(lm1)

#q7
netflix = netflix %>%
  mutate(netflix_return = (NFLX-lag(NFLX))/lag(NFLX),
         SP_return = (SP500 - lag(SP500))/lag(SP500))
beta = lm(netflix_return ~ SP_return, data = netflix)
summary(beta)

# q9
1.1961 -1 
0.1961 / 0.3407884

# q10
summary(beta)






