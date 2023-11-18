install.packages("auk")
install.packages("sf")
install.packages("gridExtra")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("descr")
install.packages("Hmisc")
installed.packages("vegan")
installed.packages("dplyr")
install.packages("colorspace")

library(auk)
library(lubridate)
library(sf)
library(gridExtra)
library(tidyverse)
library(descr)
library(Hmisc)
library(vegan)
library(dplyr)



setwd("~/Desktop/eBird/")

###Fulton 2018 was downloaded directly from eBird Fulton County is 121 201801-201812
data2018<- read.delim("~/Desktop/eBird/ebd_US-GA-121_201801_201812_relMar-2021/ebd_US-GA-121_201801_201812_relMar-2021.txt")


#####Appendix code

# Format Date
data2018$OBSERVATION_DATE <- as.Date(data2018$OBSERVATION.DATE, format="%Y-%m-%d")

# add year to the dataframe
data2018$YEAR <- year(data2018$OBSERVATION.DATE)

# add all the columns needed for the analysis (that don't vary within checklist)
sampling_event_info <- data2018 %>%
  select(SAMPLING.EVENT.IDENTIFIER, LOCALITY, LOCALITY.ID, OBSERVATION.DATE,
         PROTOCOL.TYPE, ALL.SPECIES.REPORTED, EFFORT.DISTANCE.KM, EFFORT.AREA.HA, 
         DURATION.MINUTES, YEAR, GROUP.IDENTIFIER, LATITUDE, LONGITUDE) %>%
  distinct()


# Counts how many 'x's per checklist
X_missing <- data2018 %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION.COUNT=="X"))


# accounts for the instance in which people submit 
# the same species both at the species and subspecies level
# also makes it so only 'species' and 'issf' category are included in analysis
##joins the sampling_event_info and the X_missing data all into one dataframe

data2018_clean <- data2018 %>%
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
analysis_data <- data2018_clean %>%
  ## filter for only complete checklists
  filter(ALL.SPECIES.REPORTED == 1)

 
   ## only using stationary, traveling, and exhaustive area type checklists
  analysis_data <- analysis_data  %>% filter(PROTOCOL.TYPE %in% c("Area", "Stationary", "Traveling"))
 
  
   ## Get rid of any checklists that had a single X
  analysis_data <- analysis_data  %>%   filter(number_X==0) %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>%
  summarise(Species_Richness=length(unique(COMMON.NAME)), 
            Species_Diversity=diversity(OBSERVATION.COUNT), 
            Species_Abundance=sum(OBSERVATION.COUNT, na.rm=TRUE), 
            Minutes=mean(DURATION.MINUTES, na.rm=TRUE), 
            Distance_km=mean(EFFORT.DISTANCE.KM, na.rm=TRUE), 
            Area_ha=mean(EFFORT.AREA.HA, na.rm=TRUE)) %>%
  inner_join(data2018_clean, ., by="SAMPLING.EVENT.IDENTIFIER")%>%
 
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
  filter(DURATION.MINUTES >= 5 & DURATION.MINUTES <=240) %>%
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




# Then create a matrix of plotID by species using the piping function (%>%)
# to carry out multiple steps at a time using the date in the step before
# Number of each species detected per plot in 2016
colnames(analysis_data.all)
brd_matrix<- analysis_data.all %>% group_by(SAMPLING.EVENT.IDENTIFIER, COMMON.NAME) %>%
  summarise(count=n()) %>%
  spread(COMMON.NAME,count)

# Convert NAs to 0
brd_matrix[is.na(brd_matrix)] <- 0

# Look at it - does it look ok?
brd_matrix


# Now we are ready to calculate some diversity indices by SAMPLING.EVENT.IDENTIFIER
# http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/diversity.html

### Going to save the SAMPLING.EVENT.IDENTIFIER in a separate dataframe so I can add it back in and join everything back together
subSEI <- subset (brd_matrix, select = "SAMPLING.EVENT.IDENTIFIER")

### Need to get rid of the non-numeric SAMPLING.EVENT.IDENTIFIER
brd_matrix <- brd_matrix[,-1]


### Species richness
rich <- specnumber(brd_matrix, MARGIN = 1)


#### Shannon's Biodiversity Index
shannondiv <- diversity(brd_matrix, MARGIN = 1)



######################################
######################################
######################################


##Need to take richness list and make it a dataframe
rich_df <- enframe(rich, name = NULL, value= "value") 
###Rename value to "richness"
rich_df <- rich_df %>% rename (sp_richness = value)


###Now I'm going to add back in the SAMPLING.EVENT.IDENTIFIER information back to the species number

rich_df <- cbind (rich_df,subSEI)

##Now put the richness back with all the rest of the data and get rid of any extra rows that may produce
analysis_data.all<- right_join(rich_df, analysis_data.all)
analysis_data.all<- unique(analysis_data.all) ### Though there should not be any extra rows or duplications at this point so this is probably completely redundant

#### Now do the same thing for Shannon's
##Need to take richness list and make it a dataframe
shan_df <- enframe(shannondiv, name = NULL, value= "value") 
###Rename value to "richness"
shan_df <- shan_df %>% rename (shannon = value)


###Now I'm going to add back in the SAMPLING.EVENT.IDENTIFIER information back to the species number

shan_df <- cbind (shan_df,subSEI)

##Now put the richness back with all the rest of the data and get rid of any extra rows that may produce
analysis_data.all<- right_join(shan_df, analysis_data.all)
analysis_data.all<- unique(analysis_data.all) ### Though there should not be any extra rows or duplications at this point so this is probably completely redundant


### So right now

##################
###ArcGIS part
###Make just coordinates and LOCALITY.ID

arc <-subset (analysis_data.all, select = c("LOCALITY.ID", "LATITUDE", "LONGITUDE"))
arc <- unique(arc)

write.csv(arc, 
          "~/Downloads/arc.csv", 
          row.names=F)
###### 


library(readxl)
red_ATL_bird_corrected <- read_excel("~/Desktop/eBird/red_ATL_bird_corrected.xlsx")

View(red_ATL_bird_corrected) 

colnames(red_ATL_bird_corrected)

subred <- subset (red_ATL_bird_corrected, select = c("LOCALITY.ID", "Grade"))


red<- right_join(analysis_data.all, subred)
red <- unique(red)

colnames(red)

red$sp_richness <- as.numeric(red$sp_richness)

str(red$sp_richness)

freq(red$"Grade")

red <- subset(red, !is.na(Grade))
red <- subset (red, !is.na(sp_richness))
red <- subset (red, !is.na(shannon))

mean(red$sp_richness, na.rm=TRUE)

sppr_aov <- aov(sp_richness ~ Grade, data = red)
summary(sppr_aov)

tukey.test <- TukeyHSD(sppr_aov)
tukey.test



#############################################################################


## Now we can graph it

p <- ggplot(data = subset(red, !is.na(Grade)), aes(Grade, sp_richness))
p + geom_boxplot()




sppdiv_aov <- aov(shannon ~ Grade, data = red)
summary(sppdiv_aov)


tukey.test <- TukeyHSD(sppdiv_aov)
tukey.test
### Again highly significant, and we can graph it, but first...

p2 <- ggplot(red, aes(Grade, shannon))
p2 + geom_boxplot()

colnames(red)


write.csv(red, 
          "~/Downloads/red.csv", 
          row.names=F)
######  SUMMARY STAT TABLE




