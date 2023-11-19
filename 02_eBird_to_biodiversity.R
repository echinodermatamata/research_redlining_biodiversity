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
