

install.packages("auk")
install.packages("sf")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("descr")
install.packages("Hmisc")
install.packages("vegan")
install.packages("dplyr")
install.packages("colorspace")
install.packages("tidyr")

library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(descr)
library(Hmisc)
library(vegan)
library(dplyr)
library(tidyr)



setwd("~/Documents/GitHub/research_redlining_biodiversity")

###eBird data is downloaded directly for each county 
data<- read.delim("~/Documents/GitHub/research_redlining_biodiversity/data/ebd_US-GA-089_202001_202012_relMar-2022.txt")

colnames(data)
#####Appendix code

# Format Date
data$OBSERVATION_DATE <- as.Date(data$OBSERVATION.DATE, format="%Y-%m-%d")

# add year to the dataframe
data$YEAR <- year(data$OBSERVATION.DATE)

colnames(data)

# add all the columns needed for the analysis (that don't vary within checklist)
sampling_event_info <- data %>%
  select(SAMPLING.EVENT.IDENTIFIER, LOCALITY, LOCALITY.ID, OBSERVATION.DATE,
         PROTOCOL.TYPE, ALL.SPECIES.REPORTED, EFFORT.DISTANCE.KM, EFFORT.AREA.HA, 
         DURATION.MINUTES, YEAR, GROUP.IDENTIFIER, LATITUDE, LONGITUDE) %>%
  distinct()

colnames(sampling_event_info)

# Counts how many 'x's per checklist
X_missing <- data %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION.COUNT=="X"))


# accounts for the instance in which people submit 
# the same species both at the species and subspecies level
# also makes it so only 'species' and 'issf' category are included in analysis
##joins the sampling_event_info and the X_missing data all into one dataframe

data_clean <- data %>%
  filter(CATEGORY %in% c("species","issf")) %>% 
  group_by(SAMPLING.EVENT.IDENTIFIER, COMMON.NAME) %>%
  summarise(COUNT.SPP = sum(as.numeric(as.character(OBSERVATION.COUNT)))) %>%
  rename(OBSERVATION.COUNT = COUNT.SPP) %>% 
  inner_join(., sampling_event_info, by="SAMPLING.EVENT.IDENTIFIER")%>%
  inner_join(., X_missing, by="SAMPLING.EVENT.IDENTIFIER")

####NAs introduced by coercion
warnings()

##################################################
##### apply some basic criteria to filter by #####
##################################################

# apply some filtering criteria and join into one big file
analysis_data <- data_clean %>%
  ## filter for only complete checklists
  filter(ALL.SPECIES.REPORTED == 1)


## only using stationary, traveling, and exhaustive area type checklists
analysis_data <- analysis_data  %>% filter(PROTOCOL.TYPE %in% c("Area", "Stationary", "Traveling"))


## Get rid of any checklists that have X and calculate Species Richness, Diversity, and Abundance
analysis_data <- analysis_data  %>%   filter(number_X==0) %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  summarise(Species_Richness=length(unique(COMMON.NAME)), 
            Species_Diversity=diversity(OBSERVATION.COUNT), 
            Species_Abundance=sum(OBSERVATION.COUNT, na.rm=TRUE), 
            Minutes=mean(DURATION.MINUTES, na.rm=TRUE), 
            Distance_km=mean(EFFORT.DISTANCE.KM, na.rm=TRUE), 
            Area_ha=mean(EFFORT.AREA.HA, na.rm=TRUE)) %>%
  inner_join(data_clean, ., by="SAMPLING.EVENT.IDENTIFIER")%>%
  
  ## Filter between 2010 and 2020-- not always necessary, but part of the larger code
  
  filter(YEAR>= 2010 & YEAR <= 2020)


##############################################################################
## filtering out group_identifier data to eliminate 'duplicated' checklists ##
##############################################################################

# first select the group_identifiers and associated checklists
duplicated <- analysis_data %>%
  drop_na(GROUP.IDENTIFIER) %>%
  select(GROUP.IDENTIFIER, SAMPLING.EVENT.IDENTIFIER) %>%
  distinct(.keep_all=TRUE) %>%
  group_by(GROUP.IDENTIFIER) %>%
  # randomly sample one checklist for each group_identifier
  sample_n(., 1) %>%
  .$SAMPLING.EVENT.IDENTIFIER

duplicated_data <- analysis_data %>%
  filter(SAMPLING.EVENT.IDENTIFIER %in% duplicated)

## now, append the selected checklists for each group_identifier
## with the non group_identifier checklists from the data
analysis_data <- analysis_data %>%
  filter(!grepl("G", GROUP.IDENTIFIER)) %>%
  bind_rows(., duplicated_data)


#########################################################
############ apply distance and duration caps ###########
#########################################################
analysis_data <- analysis_data %>%
  filter(DURATION.MINUTES >= 15 & DURATION.MINUTES <=240) %>%
  filter(EFFORT.DISTANCE.KM <= 10)


## rename analysis_data to signify it is the 'complete' checklist usage
analysis_data.all <- analysis_data


######################################################################
#### get rid of species which did not occur on >95% of checklists ####
######################################################################

## Exclude the species that rarely occur
checklists_hotspots <- analysis_data.all%>%
  group_by(LOCALITY.ID)%>%
  summarise(total_checklists=length(unique(SAMPLING.EVENT.IDENTIFIER)))

## create a dataframe which removes the species that are on <=5% of checklists in a hotspot
analysis_data.95 <- analysis_data.all%>%
  group_by(LOCALITY.ID, COMMON.NAME)%>%
  summarise(species_count=length(COMMON.NAME))%>%
  inner_join(checklists_hotspots, ., by="LOCALITY.ID")%>%
  mutate(percentage_of_checklists=(species_count/total_checklists)*100)%>%
  inner_join(analysis_data.all, ., by=c("LOCALITY.ID", "COMMON.NAME"))%>%
  filter(percentage_of_checklists >=5.00) ## removing species that are on < 5% of checklists in a hotspot




### Wanting to just add a few details that may be important later in analysis....maybe. 
###Using the analysis that removed the rarest species 

analysis_data.95$OBSERVATION.DATE <- ymd(analysis_data.95$OBSERVATION.DATE)



analysis_data.all <- as_date(analysis_data.95$OBSERVATION.DATE)

analysis_data.all <- analysis_data.95 %>%
  mutate(
    month = month(OBSERVATION.DATE),
    day_of_year = yday(OBSERVATION.DATE))


### Dates to Seasons
analysis_data.all <- analysis_data.all %>% mutate(season =
                                                    case_when(month == 1 ~ "W", 
                                                              month == 2 ~ "W",
                                                              month == 3 ~ "Sp",
                                                              month == 4 ~ "Sp", 
                                                              month == 5 ~ "Sp",
                                                              month == 6 ~ "Su",
                                                              month == 7 ~ "Su", 
                                                              month == 8 ~ "Su",
                                                              month == 9 ~ "F",
                                                              month == 10 ~ "F", 
                                                              month == 11 ~ "F",
                                                              month == 12 ~ "W")
)



community_by_observation <- analysis_data.all


### So now create a dataframe that is just the locality id, lat, long, species richness, shannon's, & abundance

colnames(community_by_observation)

location <- subset(community_by_observation, select= c("SAMPLING.EVENT.IDENTIFIER", "LOCALITY.ID", "LATITUDE", "LONGITUDE", "Species_Richness", "Species_Diversity","Species_Abundance" ))
colnames(location)

location <- unique(location)





