# Daily Assignment 26: Rasters

# Reading in Data 

library(terra)

url <- 'https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/foco-elev-cm.tif'
vsi_url <- paste0("/vsicurl/", url)

elev_raster <- rast(vsi_url)

# Describing the structure

print(elev_raster)

    # The raster represents elevation data of Fort Collins in centimeters as a SpatRaster.
    # Each cell corresponds to a geographic location and contains an elevation 
    # value in centimeters. It's resolution is 30,30 in cm. 

# Convert Elevation to Feet

elev_raster_feet <- elev_raster * 0.0328084

# Extract values to data frame

elev_df <- as.data.frame(values(elev_raster_feet), dataframe = TRUE)
colnames(elev_df) <- "elevation_ft"  # Rename column for clarity

# Create a density plot

library(ggpubr)
library(ggplot2)

p <- ggdensity(elev_df, x = "elevation_ft", 
               fill = "lightblue", color = "black",
               add = "mean", rug = TRUE) +
  labs(title = "Density Plot of Elevation in Feet",
       x = "Elevation (ft)",
       y = "Density") +
  theme_minimal()

print(p)

# Save as a png using ggsave

ggsave("elevation_density_plot.png", plot = p, width = 8, height = 5, dpi = 300)

