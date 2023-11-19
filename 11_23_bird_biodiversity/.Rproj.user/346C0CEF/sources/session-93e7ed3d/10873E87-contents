# Load necessary libraries
library(tidycensus)


library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)

# Set your Census API key
census_api_key("ca98ed282d04f9e94ec09dbcccbf7583fcb703a7", install=TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# Load point data from CSV file
point_data <- location

# Create an sf object for point data and set a CRS for it
library(sf)
st_crs(4326)

point_sf <- st_as_sf(point_data, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
st_crs(point_sf) <- 4326



#check the CRS for the two dataframes
st_crs(poverty)
st_crs(point_sf)

#transform them so that they are compatible
point_sf <- st_transform(point_sf, crs = st_crs(poverty))



# Merge the point data with the Census tract data
merged_data <- st_join(poverty, point_sf)

#Just to check....
colnames(merged_data)

# Create the plot
ggplot() +
  geom_sf(data = merged_data, aes(fill = percent_in_poverty), color = "white", size = 0.2) +
  geom_sf(data = point_sf, color = "red", size = 0.5) +
  scale_fill_viridis_c() + # You can use other color scales
  labs(title = "Percent in poverty with Water Quality Station Locations") +
  theme_minimal()


summary(merged_data)


#Cleaning the Data
# Remove NAs
cleaned_df <- merged_data[!is.na(merged_data$percent_in_poverty) & !is.na(merged_data$proportion_in_poverty) & !is.na(merged_data$Species_Richness) & !is.na(merged_data$Species_Diversity) & !is.na(merged_data$Species_Richness), ]

#If you have character (chr) columns that you want to convert to numeric (num) in a data frame, you can use the mutate function from the dplyr package. Here's an example:

# you want to convert the column to to numeric


cleaned_df <- cleaned_df %>%
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue))





summary(cleaned_df$ResultMeasureValue)

# basic scatterplot
ggplot(cleaned_df, aes(x = percent_in_poverty, y = Species_Richness)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Fit a linear regression line
  labs(
    title = "Species Richness by Percent Poverty in Census Tract ",
    x = "Percent in Poverty",
    y = "Species Richness"
  ) 

# If you want to extract statistics, you can use the lm() function
model <- lm(Species_Richness ~ percent_in_poverty, data = cleaned_df)

# Print the summary of the linear regression model
summary(model)