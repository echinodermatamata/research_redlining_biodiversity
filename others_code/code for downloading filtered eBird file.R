install.packages("auk")
library(auk)
library(dplyr)

auk_set_ebd_path("/Volumes/March 2020 Elements /eBird", overwrite=TRUE)

auk_ebd("/Volumes/March 2020 Elements /eBird/ebd_relMar-2021.txt")

output_file <- "2018_dekalb.txt"
###Set-up filer###

ebird_data <- auk_ebd("/Volumes/March 2020 Elements /eBird/ebd_relMar-2021.txt") %>%
  auk_country(country = "US") %>%
  auk_date(date = c("2018-01-01","2018-12-31" )) %>%
  auk_state(state = "US-GA") %>%
  auk_county(county = "Dekalb") %>%
  auk_complete()
ebird_data


#### Apply filter to file#####
ebird_data <- auk_ebd("/Volumes/March 2020 Elements /eBird/ebd_relMar-2021.txt") %>%
  auk_country(country = "US") %>%
  auk_date(date = c("2018-01-01","2018-12-31" )) %>%
  auk_state(state = "US-GA") %>%
  auk_county(county = "Dekalb") %>%
  auk_filter(file = output_file)

 
  