install.packages("tidycensus")
install.packages("tidyverse")
install.packages("sf")
install.packages("RColorBrewer")
install.packages("mapview")

library(tidycensus)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(mapview)
library(ggplot2)


# Set your Census API key
census_api_key("ca98ed282d04f9e94ec09dbcccbf7583fcb703a7", install=TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")




# download the data from the ACS using the get_acs method from tidycensus
# 
# the B05010_002E variable refers to the count of residents who live in
# households with household income below the poverty line; the B05010_001E
# variable refers to the count of residents for whom household income was
# ascertained by the ACS, e.g. the relevant denominator.
# 

poverty <- get_acs(
  state = 'GA', # Georgia state FIPS code is '13'
  county = 'DeKalb', # Specify the county name
  geography = 'tract',
  year = 2019, # 2015-2019 5-year ACS
  geometry = TRUE,
  keep_geo_vars = TRUE,
  variables = c(
    in_poverty = 'B05010_002E',
    total_pop_for_poverty_estimates = 'B05010_001E', 
    median_age = 'B01002_001E', 
    median_household_income = 'B19013_001E',
    urban_area_population = 'B01001_002E',
    housing_units = "B25001_001E",
    vacancy_status = "B25002_001E",
    median_household_income ="B19013_001E", 
    total_population = "B01003_001E",          # Total population
    white_alone = "B02001_002E",               # White alone
    black_alone = "B02001_003E",	
    native_alone = 'B02001_004E'))






# we're going to recode the variable names to more human-readable names to 
# make it easier to work with the data in subsequent steps
poverty <- poverty %>% 
  mutate(
    variable = recode(variable,
                      # you may notice that tidycensus drops the 'E' from the 
                      # end of the variable code names
                      B05010_002 = 'in_poverty',
                      B05010_001 = 'total_pop_for_poverty_estimates', 
                      B01002_001 = 'median_age', 
                      B19013_001 =  'median_household_income',
                      B01003_001 = 'total_population',
                      B01001_002 = 'urban_area_population',
                      B25001_001 = 'housing_units',
                      B25002_001 =  'vacancy_status',
                      B19013_001 = 'median_household_income',
                      B02001_002 = 'white_alone',               # White alone
                      B02001_003 = 'black_alone' ,
                      B02001_004 = 'native_alone'))


# pivot the data wider so that the in_poverty and
# total_pop_for_poverty_estimates; this follows the "tidy" format and approach
# where each row corresponds to an observation.
# 
# because the pivot_wider method can mess up your data when your data contains
# geometry/shapefile information, we will remove the geomemtry information
# and add it back in later
poverty_geometry <- poverty %>% select(GEOID) %>% unique() # save the geometry data
poverty <- poverty %>% 
  sf::st_drop_geometry() %>% # remove geometry data
  tidyr::pivot_wider(
    id_cols = c(GEOID,ALAND, AWATER),
    names_from = variable,
    values_from = c(estimate, moe))




poverty <- poverty %>% 
  mutate(
    proportion_in_poverty = estimate_in_poverty / estimate_total_pop_for_poverty_estimates,
    percent_in_poverty = proportion_in_poverty * 100)





# add the geometry back in -- 
# make sure to merge the data into the sf object with the sf object on the 
# left hand side so the output has the sf type including your geometry data
poverty <- poverty_geometry %>% 
  left_join(poverty)

st_crs(poverty)

# visualize our point estimates 
poverty1 <- ggplot(poverty, aes(fill = proportion_in_poverty)) +
  geom_sf()+
  scale_fill_viridis_c(label = scales::percent_format(),
                       limits = c(0, 1)) +
  labs(fill = "Percent in Poverty") +
  ggtitle("Poverty Estimates in Dekalb County, Georgia",
          subtitle = "Based on American Community Survey 2015-2019 Estimates")

poverty1

ggsave("poverty.jpg", plot = poverty, dpi = 300)



# finding other census tract data that may be of interest
# Load variables for a specific dataset and year (e.g., 2019 5-year ACS for Colorado)
variables <- load_variables(2019, "acs5", cache = TRUE)
