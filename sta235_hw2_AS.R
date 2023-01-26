library(tidyverse)
library(mosaic)
options(scipen = 2) 

# Fit a model that regresses monthly rent for an Austin apartment on the 
# area of the apartment, the number of bathrooms in the apartment, and the 
# age of the apartment
multReg = lm(Rent ~ Area + Bathrooms + Age, data=rents)
summary(multReg)


# QUESTION 1
# Use the multiple regression model to predict monthly rent for a two-bathroom, 
# 1000-square foot apartment in a building that is 10 years old. Round to the nearest dollar.
newData = data.frame(Area = 1000, Bathrooms = 2, Age = 10)    
predict(multReg, newdata = newData, interval = 'predict')  
# 1487.682 --> 1488


# QUESTION 2 
# Interpret the meaning of the bathrooms coefficient
coef(multReg)
# For every additional bathroom, the price of rent increases by approx $190.3066332


# QUESTION 3
# Interpret the meaning of the F-test for overall model significance:
  # What null hypothesis is being tested?
      # Null = all coefficients are zero
  # What is the numerical value of the F-statistic for this test?
      # 75.36
  # What is the p-value for this test?
      # < 2.2e-16
  # Do you reject or fail to reject the null hypothesis? Justify your answer. 
      # reject the null hypothesis bc small p value

      
# QUESTION 4
# Which of the following model coefficient(s) is/are statistically significant 
# (using a threshold of α=.05)? [bathroom,area,age] 
  # significant (𝑝< 0.05)
  summary(multReg)
  # Area --> 2.15e-10
  # Bathrooms --> 0.002021
  # Age --> 0.640601 (not significant)
  
  
# QUESTION 5 
# Construct a 95% confidence interval for the parameter that converts additional (slope)
# area into additional rent dollars within the population of 10-year old apartments 
# that have 2 bathrooms. What is the upper limit for this interval? Round to the nearest (0.0001)
newFive = data.frame(Area = 1000, Bathrooms = 2, Age = 10)
predict(multReg, newdata = newFive, interval = 'confidence')  


# QUESTION 6
# Which of the following is an appropriate interpretation of R2 for the multiple regression model?
# Multiple R-squared:  0.801
# 80 % of the variation in the apartment rent may be explanied by sqft, building age, and num bathrooms


# QUESTION 7
# Is there a statistically significant relationship between rent and the number of bathrooms?
summary(rentPriceBath)
# yes statistically significant bc pval = 8.38e-13


# QUESTION 8
# Are the results of this model practically significant in the context of predicting apartment 
# rent costs? Use at least two statistics from the model to make your argument.
# This model is practically significant in the context of predicting apartment rent costs bc
# pval & r squared
summary(multReg)


# QUESTION 9
# A developer has a brand-new apartment building under construction. 
# Apartment 14B has 500 square feet and one bathroom. Use the multiple regression model 
# to construct a 95% prediction interval for the rent on this unit.
# What is the upper bound of this interval? Round to the nearest dollar.
newApt = data.frame(Area = 500, Bathrooms = 1, Age = 0)    
predict(multReg, newdata = newApt, interval = 'predict')  
# upr = 1166.873

# QUESTION 10 
# The effect of removing building age from the model could be assessed by running an additional 
# regression of monthly rent on only the area of the apartment and the number of bathrooms in the 
# apartment. WITHOUT running such an additional regression, evaluate whether you think the removal 
# of age from the model would have a significant impact on the two most important statistics that 
# are used to assess how well the model fits the data. Explain your answer in a few sentences. 
# use summary(multReg) and summary(newReg) to tell difference

# It wont have that much of an impact. Results are less narrowed-down 
# look towards r squared and rmse

