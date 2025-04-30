# Daily Exercise 25

# Libraries
library(sf)
library(dplyr)
library(ggplot2)
library(AOI)

# Read in the river shapefile
rivers <- read_sf("MajorRivers.shp")

# Filter to Mississippi
    # Look at attribute names
unique(rivers$NAME)

    # Filter for Mississippi River system - assuming "Mississippi" appears in 'NAME'
mississippi_system <- rivers %>%
  filter(grepl("Mississippi", NAME))

# Get county boundaries
counties <- counties(cb = TRUE)  # Download all US counties

# Identify Counties Intersecting Mississippi System
    # Transform counties to match rivers
counties <- st_transform(counties, crs = st_crs(mississippi_system))

    # Intersection
intersecting_counties <- st_filter(counties, mississippi_system)

# Make a map of the counties that intersect the Mississippi River system, 
# along with the rivers themselves.
ggplot() +
geom_sf(data = intersecting_counties, fill = "lightblue") +
  geom_sf(data = mississippi_system, color = "blue") +
  theme_minimal()

# Getting US cities 
US_cities <- readr::read_csv("uscities.csv") 

    # Making a data frame of US cities from coordinates
df <- data.frame(
  city = US_cities$city,
  state = US_cities$state_name,
  population = US_cities$population,
  X = US_cities$lng,
  Y = US_cities$lat
) |>
  filter(!state %in% c("Hawaii","Alaska","Puerto Rico"))

    # Making that data frame into a spatial object
    # Geographic Coordinate System (GCS)
df_sf_gcs <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)
Cities <- st_transform(df_sf_gcs, crs = 4326)
Cities <- st_transform(Cities, crs = st_crs(mississippi_system))

# Join cities to counties 
cities_in_counties <- st_join(Cities, intersecting_counties, join = st_within)

# Sum the population by county
    # Group by county and sum population
county_population <- cities_in_counties %>%
  group_by(NAME) %>%  # 'NAME' might be the county name field, check your data
  summarise(urban_population = sum(population, na.rm = TRUE))

    # Join summed population back to counties
intersecting_counties <- intersecting_counties %>%
  st_join(county_population)

# Make the final colored map
library(viridis)

ggplot() +
  geom_sf(data = intersecting_counties, aes(fill = urban_population)) +
  geom_sf(data = mississippi_system, color = "blue") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(fill = "Urban Population", title = "Urban Population along \n Mississippi River System")



