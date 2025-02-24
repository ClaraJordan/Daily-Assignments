# Author: Clara Jordan
# Date: February 17, 2025
# Purpose of script: To complete daily assignment 7 for ESS 330, working with ggplot. 
#
###### Start Script
# Read in the COVID data
library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
#
# Load necessary packages
library(dplyr)
#
##### Question 1: Make a faceted line plot (geom_line) of the 6** states with most cases. 
               # Your X axis should be the date and the y axis cases.**
  # 1A: Identify the six states with the most current cases
top_states <- covid |>
  group_by(state) |>
  summarise(total_cases = sum(cases, na.rm = TRUE)) |>
  arrange(-total_cases) |>
  top_n(6, total_cases)
    # Answer: California, Texas, Florida, New York, Illinois, and Pennsylvania have 
    #         the most cases, respectively. 
  # 1B: Filter the raw data to those 6 states 
filtered_data <- covid |>
  filter(state %in% top_states$state)
  # 1C: Set up a ggplot –> add layers –> add labels –> add a facet –> add a theme
ggplot(data = filtered_data) +
  aes(x = date, y = cases) +
  geom_line(aes(color=state)) + 
  labs(title = "Cumulative Case Counts in the COVID-19 Pandemic",
       x = "Date",
       y= "Total Cases", 
       caption = "The top six states in the United States.", 
       size = 12) +
  theme_dark()
  # 1D: save the image to you img directory (hint: ggsave())
##### Question 2: Make a column plot (geom_col) of daily total cases in the USA. 
               # Your X axis should be the date and the y axis cases.
  # 2A: Identify the total cases each day in the whole country (hint: group_by(date))
daily_total_cases <- covid |>
  group_by(date) |>
  summarise(total_cases = sum(cases, na.rm = TRUE))
  # 2B: Set up a ggplot –> add layers –> add labels –> add a theme
ggplot(data = daily_total_cases) +
  aes(x = date, y = total_cases) +
  geom_col()
  labs(title = "Case Counts in the COVID-19 Pandemic Per Day",
       x = "Date",
       y= "Cases",  
       size = 12) +
  theme_dark()
  # 2C: Save the image to your img directory (hint: ggsave())



  
  
