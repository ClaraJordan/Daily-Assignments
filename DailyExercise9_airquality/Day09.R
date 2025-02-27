# Name: Clara Jordan
# Date: February 24, 2025
# Purpose of Script: To gain an introduction to regression modelling in R through
# examining, cleaning, and plotting an air quality dataset.
#
###### Questions to be answered
# 1. Use the help (?) function to learn more about the dataset (airquality)
# 2. Use vis_dat to check out the data. Does it need cleaning?
# 3. Fit a linear model to the cleaned data to predict Ozone from one of the 
#     possible predictors of your choosing. Why did you chose thats variable?
# 4. Using summary(), Does this seem like a valid model?
# 5. Explain the R2 found in a sentence.
# 6. Use broom::augment to predict the Ozone of the cleaned data
# 7. Use ggplot to plot the actual vs predicted Ozone
# 8. Add a red line to show where the actual and predicted values are equal. 
#     This can be done by plotting a 1:1 line (e.g. intercept 0, slope 1) with 
#     geom_abline(intercept = 0, slope = 1, color = "red")
# 9. Add a subtitle to the plot showing the correlation between the actual and 
#     predicted values are equal This can be done by plotting a 1:1 line 
#     (e.g. intercept 0, slope 1) with
# 10. paste("Correlation:", round(cor(a$Ozone, a$.fitted),2)) assuming your 
#     augmented data object is called a
#
###### Start Script

library(tidyverse)
library(tidymodels)
install.packages("visdat")
library(visdat)
library(broom)

### 1. Use the help (?) function to learn more about the dataset

?airquality 

  # Highlights from the dataset
    # Daily air quality measurements in New York, May to September 1973.
    # A data frame with 153 observations on 6 variables.
     # [,1]	Ozone	numeric	Ozone (ppb)
     # [,2]	Solar.R	numeric	Solar R (lang)
     # [,3]	Wind	numeric	Wind (mph)
     # [,4]	Temp	numeric	Temperature (degrees F)
     # [,5]	Month	numeric	Month (1--12)
     # [,6]	Day	numeric	Day of month (1--31)

### 2. Use vis_dat to check out the data. Does it need cleaning?

vis_dat(airquality)

  # Answer: It looks like this dataset does need some cleaning, and has missing 
  # data for both ozone and solar radiation. All other observation categories
  # seem to have all their observations. 

  # Removal of missing data

airquality_cleaned <- airquality |> 
  drop_na()

vis_miss(airquality_cleaned) # Check to see if there is missing data still. 

### 3. Fit a linear model to the cleaned data to predict Ozone from one of the 
#      possible predictors of your choosing. Why did you chose that variable?

model <- lm(Ozone ~ Solar.R, data = airquality_cleaned)

  # I chose temperature as my predictor variable because I know that 
  # tropospheric ozone tends to increase with photochemical reactions between
  # transport emissions (e.g. N2O and VOCs) and solar radiation. Thus, I think
  # there should be a direct relationship between Solar.R and Ozone. 

### 4. Using summary(), Does this seem like a valid model?

summary(model)

  # Answer: This does not seem like a valid model. My R^2 value is extremely low
  # at 0.1213, and the adjusted R^2 is even lower at 0.1133. The residual standard
  # error is also quite high at 31.33. Additionally, 
  # this model has very high residuals (ranging frim -48 to 119) and a large 
  # standard error of 6.7 for the intercept. 

### 5. Explain the R2 found in a sentence.

  # Answer: The low R^2 value, 0.1213, indicates that there is 
  # very little relationship between Solar.R and Ozone, and very little of the 
  # variance in Ozone observations is explained by solar radiation. 

### 6. Use broom::augment to predict the Ozone of the cleaned data. 

augmented_data <- augment(model, airquality_cleaned)
head(augmented_data)

### 7. Use ggplot to plot the actual vs predicted Ozone

ggplot(augmented_data, aes(x = Ozone, y = .fitted)) +
  geom_point() +
  labs(x = "Actual Ozone", y = "Predicted Ozone", title = "Actual vs Predicted Ozone")

### 8. Add a red line to show where the actual and predicted values are equal. 
###     This can be done by plotting a 1:1 line (e.g. intercept 0, slope 1) with 
#        geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(augmented_data, aes(x = Ozone, y = .fitted)) +
  geom_point() +
  labs(x = "Actual Ozone", y = "Predicted Ozone", title = "Actual vs Predicted Ozone") +
  geom_abline(intercept = 0, slope = 1, color = "red")

### 9. Add a subtitle to the plot showing the correlation between the actual and 
#        predicted values are equal This can be done by plotting a 1:1 line 
#       (e.g. intercept 0, slope 1) with paste("Correlation:", round(cor(a$Ozone, a$.fitted),2)) 
#       assuming your augmented data object is called a

correlation_value <- cor(augmented_data$Ozone, augmented_data$.fitted)
subtitle_text <- paste("Correlation:", round(correlation_value, 2))

ggplot(augmented_data, aes(x = Ozone, y = .fitted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual Ozone", y = "Predicted Ozone", 
       title = "Actual vs Predicted Ozone", 
       subtitle = subtitle_text)




