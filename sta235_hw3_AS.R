library(tidyverse)
library(mosaic)

# Q1: Regress Price on Brick. 
#     What does this model estimate for the difference between the mean price of 
#     brick homes and the mean price of non-brick homes? [Brick minus non-brick]
lm1 = lm(Price ~ Brick, data=MidCity)
coef(lm1)
# 25810.91

# Q2: Test the null hypothesis that the effect on Price of being a brick home is 
#     no different from the effect on Price of not being a brick home. 
#     The alternative hypothesis is that the effect is different. 
#     TRUE or FALSE: At the usual (default) standard for statistical significance, 
#     this null hypothesis is rejected.
summary(lm1)
# true

# Q3: Regress Price on Brick and SqFt (the parallel lines model). 
#     For any given SqFt, by how much does the estimated price of a brick home 
#     having that given amount of SqFt exceed the estimated price of a non-brick 
#     home having the same amount of SqFt?
lm2 = lm(Price ~ Brick + SqFt, data=MidCity)
coef(lm2)
# 23445.10

# Q4: According to the model for question 3, how much is the estimated increase 
#     in price of a brick home for each additional SqFt?
coef(lm2)
# 66.06

# Q5: Run an appropriate regression model to test the null hypothesis that the 
#     parallel lines model is correct versus the alternative hypothesis that a 
#     crossing lines model is correct. 
#     [Hint: You will need to add an interaction predictor variable to the model.] 
#     What is the numerical value of the p-value for this test?
lm3 = lm(Price ~ Brick + SqFt + Brick:SqFt, data=MidCity)
summary(lm3)
# 2.2e-16 OR 0.174


# Q6: According to the model for question 5, how much is the estimated increase 
#     in price of a brick home for each additional SqFt?
lm3 = lm(Price ~ Brick + SqFt + Brick:SqFt, data=MidCity)
coef(lm3)
# 84.2


# Q7: Run a regression of Price on Brick and Neighborhood (appropriately coded). 
#     This is an additive cross-tabs model. As your answer for this question, 
#     create and submit the additive cross-tabs table in the answer space provided here.  
#     Populate the 6 cells of the indicated cross-tabs table with the values 
#     estimated by the regression. You will have a cross-tabs table that has 
#     2 rows (for Brick) and 3 columns (for Neighborhood).  
#     Your table need not be fancy, but be sure you label the rows and columns.
lm4 = lm(Price~Brick +factor(Nbhd), data=MidCity)
summary(lm4)
confint(lm4)
# refer to image


# Q8: According to the model of question 7, what is the estimated value of 
#     123 Lotus Avenue, which is a brick home in neighborhood 3?
# 170666.09
44297.43 + 19281.69 + 107087.00

# Q9: Use the predict function in R and the model of question 7 to make a 95% 
#     prediction interval for the price of 123 Lotus Avenue, which is a brick 
#     home in neighborhood 3. Write the numerical value of the upper limit of 
#     this prediction interval. In an attempt both to gain deeper insight and to 
#     increase the explanatory power of the model, we will add additional predictor 
#     variables to the model.  Run the following regression model and use the 
#     results to help answer the remaining questions 
new_data = data.frame(Brick = "Yes", Nbhd = 3)
predict(lm4, newdata = new_data, interval = 'predict')
# 202101.8

# Q10: The regression model estimates that the price of brick homes averages 
#      $ _____ more than the price of non-brick homes that have the same values 
#      for the other predictor variables in the model. 
#      [Round your answer to the nearest dollar.]
lm5 = lm(Price~Brick +factor(Nbhd)+ Offers +SqFt + Bedrooms + Bathrooms, data=MidCity)
summary(lm5)
# 17297

# Q11: The regression model estimates that the price of homes in Neighborhood 2 
#      averages $ _____ less than the price of Neighborhood 1 homes that have the 
#      same values for the other predictor variables in the model. 
#      [Round your answer to the nearest dollar.]
# 1.561

# Q12: It is claimed that Neighborhoods 1 and 2 could be combined into a single 
#      “older” neighborhood for the purpose of price estimation because the effect 
#      on price of homes being in Neighborhood 2 is insignificantly different from 
#      the effect on price of similar homes being in Neighborhood 1. Here, "similar" 
#      means having the same values of the other predictor variables. 
#      Summarize the statistical evidence for or against this claim. What do you think?
confint(lm5)


# Q13: Use the regression output to create an additive cross-tabs table that has 
#      2 rows (for Brick) and 3 columns (for Neighborhood).  Populate the 6 cells 
#      of the table in the same manner that you did in question 7, except that 
#      you will use the parameter estimates from the longer regression equation.
lm5 = lm(Price~Brick +factor(Nbhd)+ Offers +SqFt + Bedrooms + Bathrooms, data=MidCity)
summary(lm5)


# Q14: Compare your additive table from question 13 with your additive table 
#      from question 7. In question 7, you were able to estimate the complete 
#      price of a home using only the table. You cannot estimate the complete 
#      price of a home using only the table from question 13.
#      (a) Why not?
#      (b) What does the table from question 13 allow you to estimate?
#      (c) Are both tables additive, in the sense of the difference between their 
#          rows being the same in every column, and the difference between their 
#          columns being the same in each row?

# Q15: Let us test the adequacy of the additive cross-tabs model for Brick and 
#      Neighborhoods in the longer model. Interact Brick with Neighborhoods 
#      (with both N2 and N3) and add the interactions to the longer model. 
#      Run the revised model. From the results, we learn that 
#      [pick all that apply]
lm6 = lm(Price~Brick +factor(Nbhd)+ Offers +SqFt + Bedrooms + Bathrooms +Brick: factor(Nbhd), data=MidCity)
summary(lm6)

16980.797 - 12093.056

