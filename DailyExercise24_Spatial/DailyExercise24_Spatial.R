# Daily Assignment 24: Making Spatial Data
# Date Created: April 23, 2025
# Author: Clara Jordan

# Libraries

    # spatial data science
library(tidyverse)
library(sf)
library(units)

    # Data
library(AOI)

    # Visualization
library(gghighlight)
library(ggrepel)
library(knitr)

    # Data cleaning
library(dplyr)

# 1. First go to this site and download the appropriate (free) dataset into the 
# data directory of your project.

    # Done. 

# 2. Once downloaded, read it into your working session using readr::read_csv() 
# and explore the dataset until you are comfortable with the information it contains.

US_cities <- readr::read_csv("uscities2.csv")

# 3. While this data has everything we want, it is not yet spatial. 
# Convert the data.frame to a spatial object using st_as_sf and prescribing the 
# coordinate variables and CRS (Hint what projection are the raw coordinates in?)

    # Making a data frame of US cities from coordinates
df <- data.frame(name = US_cities$state_name, 
                 X = US_cities$lng, 
                 Y = US_cities$lat) 
head(df)

    # Making that data frame into a spatial object
    # Geographic Coordinate System (GCS)
(df_sf_gcs = st_as_sf(df, 
                      coords = c("X", "Y"), 
                      crs = 4269))

# 4. Next, we will find the county boundaries for Larimer County, Colorado. 
# Use the aoi_get(state = "CO", county = "Larimer") function to get the county boundary.

larimer_boundry <- aoi_get(state = "CO", county = "Larimer")

# 5. Use st_filter to find all the cities in the dataset that are within Larimer County.

  # Matching the GCS of the two datasets
larimer_boundry <- st_transform(larimer_boundry, crs = st_crs(df_sf_gcs))

  # Filtering 
larimer_cities <- st_filter(df_sf_gcs, larimer_boundry)

# 6. Plot the cities and the county boundary on a single map. Use geom_sf() to 
# plot both layers and make sure to set the fill and color arguments appropriately.

library(ggplot2)

ggplot() +
  geom_sf(data = larimer_boundry, fill = NA, color = "black", size = 1) +
  geom_sf(data = larimer_cities, color = "blue", size = 1) +
  theme_minimal() +
  ggtitle("Cities in Larimer County, Colorado")


# 7. Next, determine the three cities in Larimer County with the highest population 
# and add these to the map. Use a larger point size and a different color for the 
# points will help them stand out.

three_largest_cities <- US_cities |> ## PASS IN SPATIAL OBJECT INSTEAD!!!!
  filter(county_name == "Larimer") |>
  arrange(desc(population)) |>
  slice_head(n = 3)

(three_largest_cities = st_as_sf(three_largest_cities, 
                      coords = c("lng", "lat"), 
                      crs = 4269))
  
# 8. If you dont already have it, install the ggrepel package. 
# This package provides a function called geom_label_repel() that will help you 
# label the points without overlapping labels. 
# The syntax is similar to geom_label() but it will automatically adjust the position 
# of the labels to avoid overlap.

ggplot() +
  geom_sf(data = larimer_boundry, fill = NA, color = "black", size = 1) +
  geom_sf(data = larimer_cities, color = "blue", size = 1) +
  geom_sf(data = three_largest_cities, color = "red", size = 3) +
  # Add labels to the cities
  ggrepel::geom_label_repel(
    data = three_largest_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3) +
  theme_minimal() +
  ggtitle("Top 3 Most Populous Cities in Larimer County")

# Like all ggplot2 layers, you can add this to your existing plot and first must 
# prescribe the data to be used.In the aesthetic mapping, you will need to specify 
# the label argument to be the name of the city. and the geometry argument to be 
# the geometry column of you sf object.Finally, you need to set the stat argument 
# to sf_coordinates so the function knows to use the coordinates of the geometry 
# column for the label positions.


