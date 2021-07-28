install.packages("auk")
install.packages("sf")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tidyverse")

library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)


input_file <- system.file("Downloads/ebd_US-GA-121_201501_202012_relMar-2021/ebd_US-GA-121_201501_202012_relMar-2021.txt", package = "auk") %>% 
  auk_ebd()
ebd

output_file <- "ebd_filtered_grja.txt"
ebd <- auk_ebd(input_file)
ebd_filters <- auk_country(ebd_filters, county = "Fulton")
ebd_filtered <- auk_filter(ebd_filters, file = output_file)
ebd_df <- read_ebd(ebd_filtered)



data61 <- read.delim("~/Downloads/ebd_US-GA-061_201001_202012_relFeb-2021/ebd_US-GA-061_201001_202012_relFeb-2021.txt")


data67 <- read.delim("~/Downloads/ebd_US-GA-067_201001_202012_relFeb-2021/ebd_US-GA-067_201001_202012_relFeb-2021.txt")

data89 <- read.delim("~/Downloads/ebd_US-GA-089_201001_202012_relFeb-2021/ebd_US-GA-089_201001_202012_relFeb-2021.txt")

data121 <- read.delim("~/Downloads/ebd_US-GA-121_201001_202012_relFeb-2021/ebd_US-GA-121_201001_202012_relFeb-2021.txt")

data135 <- read.delim("~/Downloads/ebd_US-GA-135_201001_202012_relFeb-2021/ebd_US-GA-135_201001_202012_relFeb-2021.txt")

data239 <- read.delim("~/Downloads/ebd_US-GA-239_201001_202012_relFeb-2021/ebd_US-GA-239_201001_202012_relFeb-2021.txt")

data265 <- read.delim("~/Downloads/ebd_US-GA-265_201001_202012_relFeb-2021/ebd_US-GA-265_201001_202012_relFeb-2021.txt")

data307 <- read.delim("~/Downloads/ebd_US-GA-307_201001_202012_relFeb-2021/ebd_US-GA-307_201001_202012_relFeb-2021.txt")

fulton <- data121
gwinnett <- data135
quitman <- data239
taliaferro<- data265
webster<-data307
clay<- data61
cobb <- data67
dekalb <- data89

counties <- bind_rows(fulton,gwinnett,quitman,taliaferro,webster,clay,cobb,dekalb)
### This makes your date in OBSERVATION.DATE a date that is recognized in R
counties$OBSERVATION.DATE <- as.Date(ymd(counties$OBSERVATION.DATE))

### This is from project eBird's code itself- it resolves several issues that you might encounter, so it's work doing I think
##It comes from here:  
##https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html

counties <- counties %>% 
  mutate(
    # convert X to NA
    OBSERVATION.COUNT = as.character(OBSERVATION.COUNT),
    OBSERVATION.COUNT = if_else(OBSERVATION.COUNT == "X", 
                                NA_character_, OBSERVATION.COUNT),
    # effort_distance_km to 0 for non-travelling counts
    EFFORT.DISTANCE.KM = if_else(PROTOCOL.TYPE != "Traveling", 
                                 0, EFFORT.DISTANCE.KM),
  
    # split date into year and day of year
    year = year(OBSERVATION.DATE),
    day_of_year = yday(OBSERVATION.DATE)
  )

counties <- counties %>% replace_na(list(OBSERVATION.COUNT = 1))

write.csv(counties, "~/Desktop/Data_Intensive_Ecology_2021/counties.csv")

data <- read.csv("~/Desktop/Data_Intensive_Ecology_2021/counties.csv")

colnames(data)

library(descr)
library(Hmisc)
options(stringsAsFactors=F)
label(data$OBSERVATION.COUNT)<-"Number of Birds Observed Over Five Year Period"
label(data$OBSERVATION.DATE)<-"Date Observed"
freq(data$OBSERVATION.COUNT)
freq(data$day_of_year)


data2016 <- subset (data, year == 2016, select = c('day_of_year', 'OBSERVATION.COUNT'))


label(data2016$OBSERVATION.COUNT)<-"Number of Birds Observed Over Five Year Period"
label(data2016$OBSERVATION.DATE)<-"Date Observed"
freq(data2016$OBSERVATION.COUNT)
freq(data2016$day_of_year)

data2017 <- subset (data, year == 2017, select = c('day_of_year', 'OBSERVATION.COUNT'))


label(data2017$OBSERVATION.COUNT)<-"Number of Birds Observed Over Five Year Period"
label(data2017$OBSERVATION.DATE)<-"Date Observed"
freq(data2017$OBSERVATION.COUNT)
freq(data2017$day_of_year)

data2018 <- subset (data, year == 2018, select = c('day_of_year', 'OBSERVATION.COUNT'))


label(data2018$OBSERVATION.COUNT)<-"Number of Birds Observed Over Five Year Period"
label(data2018$OBSERVATION.DATE)<-"Date Observed"
freq(data2018$OBSERVATION.COUNT)
freq(data2018$day_of_year)

data2019 <- subset (data, year == 2019, select = c('day_of_year', 'OBSERVATION.COUNT'))


label(data2019$OBSERVATION.COUNT)<-"Number of Birds Observed Over Five Year Period"
label(data2019$OBSERVATION.DATE)<-"Date Observed"
freq(data2019$OBSERVATION.COUNT)
freq(data2019$day_of_year)


data$OBSERVATION.COUNT <- as.character(data$OBSERVATION.COUNT)
data$OBSERVATION.COUNT[data$OBSERVATION.COUNT == "None"] <- "0"
data$OBSERVATION.COUNT <- as.numeric(data$OBSERVATION.COUNT)

write.csv(data, "~/Desktop/Data_Intensive_Ecology_2021/counties_corrected.csv")

