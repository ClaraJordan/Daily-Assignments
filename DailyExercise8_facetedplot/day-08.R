# Name: Clara Jordan
# Date: February 19, 2025
# Purpose of Script: To develop a faceted plot of 
#                    the cumulative cases & deaths by USA region.
#
####### Start Script
# Question 1: Make a faceted plot of the cumulative cases & deaths by USA region. 
# Your x axis should be the date and the y axis value/count.
# To do this you will need to join and pivot the COVID-19 data.
  # Instructions:
    # 1A: Read in the COVID-19 data
    # 1B: Create a new data.frame using the available state.abb, state.name, 
    #     state.region objects in base R. Be intentional about creating a primary 
    #     key to match to the COVID data!
    # 1C: Join your new data.frame to the raw COVID data. 
    #     Think about right, inner, left, or full join…
    # 1D: split-apply the joined data to determine the daily, cumulative, 
    #     cases and deaths for each region
    # 1E: Pivot your data from wide format to long
    # 1F: Plot your data in a compelling way (setup, layers, labels, facets, themes)
    # 1G: Save the image to your img directory with a good file name and extension!
#
###### Code as follows: 

library(tidyverse)
library(dplyr)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)

df = data.frame(region = state.region,
                state = state.name,
                state_abbreviation = state.abb)

joined_data <- full_join(covid, df, by = "state")

# Finding the daily cases for each region
daily_cases <- joined_data |>
  arrange(region, date) |>  
  group_by(region, date) |>
  summarize(daily_cases = sum(cases))
  
# Finding the daily deaths for each region
daily_deaths <- joined_data |>
  arrange(region, date) |>  
  group_by(region, date) |>
  summarize(daily_deaths = sum(deaths))

###### Pivot Data
daily_cases |>
  pivot_longer(cols = region)
               
###### Plot Data
joined_finaldata1 <- left_join(daily_cases, daily_deaths, by = "region", relationship = "many-to-many")

colnames(joined_finaldata1)

joined_finaldata1 |>
  ggplot(aes(x = date.x, y = count)) + 
  geom_line(aes(y = daily_cases)) +  
  geom_line(aes(y = daily_deaths, color = region)) + 
  facet_grid(.~region) +
  labs(title = "COVID-19 Case Counts by Region",
       subtitle = "Daily Cumulative Cases",
       x = "Date",
       y= "Cumulative Cases",  
       size = 12) +
  theme_light()
  


