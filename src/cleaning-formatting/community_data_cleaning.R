#######################################
#  Download and clean INCLINE data  #
#######################################

# Script by Eva Lieungh

# Clean INCLINE community data from OSF (Vandvik, V., Skarpaas, O., Telford, R. J., Gya, R., Gaudard, J., Egelkraut, D., … Töpper, J. (2022, April 27). INCLINE. Retrieved from osf.io/zhk3m )

library(tidyverse) # for data cleaning

# it's possible to use datadownloader or other ways to download the data. Here I use a manual workaround of downloading the data first, then reading it from my folder:
community <- read.csv('Data/Community/INCLINE_community_2018_2019_2021.csv') 

# this version of the data had not been cleaned properly (April 2022). This script should be updated when the final clean data are up on OSF. 

# Look at the data
#------------------------------------
community[1:5,1:15]
community[1:5,188:198] # species in cols 12-182
str(community)

# Convert to presence-absence 1-0 
#------------------------------------
community <- community %>%
  mutate(across(c(12:182), ~ifelse(.=="", 0, as.character(.)))) %>% # change blanks to 0
  mutate(across(c(12:182), ~ifelse(is.na(.), 0, as.character(.)))) %>% # change NAs to 0
  mutate(across(c(12:182), ~ifelse(.!="0", 1, as.character(.)))) # change the remaining values (F,S,J etc) to 1 (present)

# Clean up some weird things
#------------------------------------
unique(community$Site)
community <- community[-which(community$Site==""),] # remove 1 row missing data
which(community$jamne!=0) # assume 'jamne' is Sel_sel and change these
community$Sel_sel[c(8944,8964)] <- 1

# there is a lot of details to clean, like _cf species etc. Most of these will be removed when deleting 'rare species' in S0 script, but should ideally be cheched against data from additional years 

# create new columns with unique subplot ID, plot ID and block ID
community$subPlotID = paste(substr(community$Site,1,3),
                            community$Block,
                            community$plot,
                            community$subPlot,
                            sep = '_')
community$plotID = paste(substr(community$Site,1,3),
                            community$Block,
                            community$plot,
                            sep = '_')
community$blockID = paste(substr(community$Site,1,3),
                         community$Block,
                         sep = '_')

# reorder and select columns to keep
community <- community %>%
  select(Site,Block,plot,subPlot,
         blockID,plotID,subPlotID,
         Treatment,year,date,Measure,
         moss,lichen,litter,soil,rock,poo,fungus,bare, 
         logger,Veg_cover,Veg_height_mm,Moss_depth_mm, 
         Ach_mil:Vio_sp) # all the species (excluding unknown etc)
# subset only first year and relevant columns for Hmsc
community18 <- subset(community,year==2018)

# remove transplant subplots and 'cover'
unique(community18$subPlot)
community18 <- community18[!(community18$subPlot=='cover'| 
                             community18$subPlot=='9'|
                             community18$subPlot=='11'|
                             community18$subPlot=='13'|
                             community18$subPlot=='23'|
                             community18$subPlot=='25'|
                             community18$subPlot=='27'),]

# identify and remove duplicates
rows <- which(duplicated(community18$subPlotID)==TRUE)
community18 <- community18[-rows,] # In Lav block 4, plot 6 is repeated! 

# keep only relevant columns
community18 <- community18 %>%
  select(Site,blockID,plotID,subPlotID,
         Ach_mil:Vio_sp)

# save clean data
#------------------------------------
write.csv(community18,'data/Community/INCLINE_community_2018_clean.csv')

