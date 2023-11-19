library(sf)

options(sf_config = list(
  SHAPE_RESTORE_SHX = "YES"
))

# Load HOLC data (replace "holc_data.shp" with your file)
holc_sf <- st_read("~/Documents/GitHub/research_redlining_biodiversity/data/fullshpfile/shapefile/holc_ad_data.shp")

#check the CRS for the two dataframes
st_crs(holc_sf)
st_crs(cleaned_df)

#transform them so that they are compatible
cleaned_df <- st_transform(cleaned_df, crs = st_crs(holc_sf))

# Check validity
st_is_valid(holc_sf)
st_is_valid(bird_red_census)

# If invalid geometries are detected, attempt to fix them
holc_sf <- st_make_valid(holc_sf)
bird_red_census <- st_make_valid(bird_red_census)

# Merge the point data with the Census tract data & bird
bird_red_census <- st_join(holc_sf, bird_red_census)

#Just to check....
colnames(bird_red_census)

# Create the plot
ggplot() +
  geom_sf(data = bird_red_census, aes(fill = holc_grade), color = "white", size = 0.2) +
  geom_sf(data = point_sf, color = "red", size = 0.5) +
 # You can use other color scales
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