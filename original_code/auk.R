install.packages("auk")
library(auk)
library(dplyr)
system.file ("extdata/ebd-sample.txt", package = "auk")
f_in <- system.file ("extdata/ebd-sample.txt", package = "auk")
f_out <- "ebd_filtered_grja.txt"

ebd <-system.file("extdata/ebd-sample.txt", package= "auk") %>% 
  auk_ebd()
ebd

ebird_data <- f_in %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_species(species = "Canada Jay") %>% 
  auk_country(country = "Canada") %>% 
  # 3. run filtering
  auk_filter(file = f_out) %>% 
  # 4. read text file into r data frame
  read_ebd()

ebd <- auk_ebd("ebd_sampling_relMar-2021.txt")

f <- system.file("ebd_sample_relMar-2021.txt", package="auk")

ebd <-auk_ebd(f) %>%
  auk_country(country = c("US")) %>%
  auk_date(date= c("2018-01-01", "2018-12-31")) %>%
  auk_complete()
