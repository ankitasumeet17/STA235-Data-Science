# The R dataset “faithful” contains a list of 272 observations of geyser eruptions during October 1980. 
# The duration of each eruption and the waiting time between eruptions are given, measured in minutes. 
# You can summon this data set into RStudio by typing on your script: data(faithful)

library(tidyverse)
library(mosaic)
library(ggplot2)
data(faithful)

#-------------------------------------------------------------------------------

# Q1: You plan to fit a simple linear model to regress waiting time on eruptions duration. 
#     But, of course, you first plot the data before fitting the model. Make a scatterplot 
#     displaying the response-predictor relationship. What does this plot suggest? 
plot(faithful$eruptions , faithful$waiting)

#-------------------------------------------------------------------------------

# Q2: Use the Old Faithful data set to fit a simple linear model to regress waiting time on 
#     eruptions duration, then plot the model residuals against fitted values.
#     What can we conclude from this residual plot? 
plot(faithful$eruptions , faithful$waiting)
lm1 = lm(waiting ~ eruptions, data=faithful)
plot(lm1)

#-------------------------------------------------------------------------------

# Q3: Our ISLR textbook has a variety of built in data sets, including Auto, a 
#     data frame with 392 vehicle models and their characteristics including gas mileage, 
#     engine displacement , horsepower, acceleration, model year, and weight. 
#     To access these data, type on your R script:

install.packages("ISLR")
library(ISLR)
data(Auto)

#     You plan to fit a simple linear model to predict gas mileage (mpg) from horsepower. 
#     But first you naturally make a Y-X scatterplot. Based on this plot, which of the following 
#     common problems with regression or violation of model assumptions can you expect if you 
#     proceed to regress mpg on horsepower?
plot(Auto)
lm2 = lm(mpg ~ horsepower , data=Auto)
plot(lm2)
plot(Auto[c(1,4)])

#-------------------------------------------------------------------------------

# Q4: The file profs.csv  Download profs.csvdata set contains a data frame with 
#     course-instructor survey scores from 463 courses taught by 94 professors. 
#     Other variables include information about each course and its instructor 
#     including the instructor's age, gender, and a 'beauty' rating of each professor’s 
#     physical attractiveness as judged by a panel of six students. Fit a regression 
#     model to predict evals from main effects of the variables age, beauty, and 
#     gender as well as the interactions between these variables:

lm3 = lm(eval ~ age * beauty * gender , data=profs)

#     Evaluate this model with the usual plots and processes to check if model 
#     assumptions are satisfied and for common problems that arise with regression. 
#     Which of the following is true of the results of this model?
plot(lm3)

#-------------------------------------------------------------------------------

# Q5: What factors impact National Football League teams' weekly stadium attendance?* 
#     With observations for each team by week (n = 10,846) from each season during 2000--2016, 
#     we have information characterizing team and opponent status. You plan to use these data 
#     to fit a model regressing response variable weekly_attendance on predictor variables: 
#         strength_of_schedule
#         margin_of_victory
#         offensive_ranking
#         defensive_ranking
#         playoffs

#     Before using model results to construct confidence intervals and calculated p-values, 
#     you check regression assumptions. Which of the following statements is accurate with 
#     respect to the specifications for linear regression?

#-------------------------------------------------------------------------------

# Q6: What factors impact National Football League teams' weekly stadium attendance?* 
#     With observations for each team by week (n = 10,846) from each season during 
#     2000--2016, we have information characterizing team and opponent status in the 
#     NFL.csv file at this link  Download NFL.csv file at this link. 
#     You plan to use these data to fit a model regressing response variable 
#     weekly_attendance on predictor variables: 
  
#         strength_of_schedule
#         margin_of_victory
#         offensive_ranking
#         defensive_ranking
#         playoffs
#         simple_rating
#         team
#         the interaction between simple_rating and team

#     Evaluate this model with the usual plots and processes to check if model assumptions 
#     are satisfied and for common problems that arise with regression. Comment on your 
#     findings, specifically: are regression model assumptions satisfied? 
#     Give a short summary of your conclusion about each of the four linear model specifications. 
#     are there any outliers or influential points?

regmod = lm(weekly_attendance ~ strength_of_schedule + margin_of_victory + offensive_ranking + defensive_ranking + playoffs + simple_rating + team + simple_rating:team , data= NFL)
#-------------------------------------------------------------------------------

# Q7: What factors predict overnight lodging prices in Santa Fe, New Mexico? 
#     The airbnb.csv data set  Download airbnb.csv data set is a sample of 99 
#     rentals in Santa Fe, scraped from the Airbnb website with variables that 
#     include the following information for each rental property:
  
#         Price: rental price in $/night on a randomly selected day in April 2018
#         Bedrooms: number of bedrooms
#         Baths: number of bathrooms
#         PlazaDist: distance in miles to the Santa Fe Plaza
#         Cancellation: dummy variable taking on the value 1 if the host has a cancellation policy, and 0 otherwise

#     Use this data frame to regress Price on PlazaDist, Bedrooms, Baths, and Cancellation. 
#         a. Use appropriate evidence to check each of the model assumptions.
#         b. One case is influential: which one is it?
#         c. Rerun the model by holding out the influential case. 
#            Does it substantially change your interpretation of the model output?
#            If you were writing a report about this data set, would you advocate using 
#            the full data set, or holding out this case and reporting on it separately?

lm4 = lm(price ~ PlazaDist + Bedrooms + Baths + Cancellation , data=airbnb)
#-------------------------------------------------------------------------------

# Q8: This dataset contains information about 53,940 round-cut diamonds. 
#     Summon this data set into RStudio by typing on your script:
data(diamonds)
lm5 = lm(price ~ carat + cut + clarity + color , data=diamond)

#     Key variables in the data frame:
#         price: US dollars ($)  -- [$326-$18,823]
#         carat: weight of the diamond  --  [0.2-5.01]
#         cut: quality of the cut  -- {Fair, Good, Very Good, Premium, Ideal}
#         clarity: measurement of how clear the diamond is  -- {I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best)}
#         color: diamond color  -- {J (worst) to D (best)}


#     Fit a regression model to predict price in terms of carat, cut, clarity, and color.  
#     Evaluate this model with the usual plots and processes to check if model assumptions 
#     are satisfied. Which of the following can we conclude from this analysis?

  











