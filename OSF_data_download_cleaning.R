#######################################
#  Download and clean INCLINE data  #
#######################################

# Script by Eva Lieungh

# Download INCLINE community data from OSF
# (requires OSF user profile and access to INCLINE OSF repository)

# remotes::install_github("Between-the-Fjords/dataDownloader") # dataDownloader package for getting data from OSF
# install.packages('osfr')
library(dataDownloader)
library(osfr) # to access private OSF repository with token
library(tidyverse) # for data cleaning

# osf_auth(token = "code") # create code on OSF webpage under user settings > personal access tokens
osf_auth(token = "DrN9MJgKeppqUoyAW1UYR2NqoHNbH1Q1n7qoVhrPTnUXU8WAN34kpiclHkajnqz9CE0sDp")

community <- get_file(node = "zhk3m",
                      file = "INCLINE_community_2018_2019_2021.csv",
                      path = "Data/Community", #local folder to save in
                      remote_path = "RawData/Community") #OSF folder
# https://github.com/Between-the-Fjords/dataDownloader/issues/4 
community <- read.csv('Data/Community/INCLINE_community_2018_2019_2021.csv') # manual workaround

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
  # the 'else' in the functions returns the value as it was, in character format.

# Clean up some weird things
#------------------------------------
unique(community$Site)
community <- community[-which(community$Site==""),] # remove 1 row missing data
which(community$jamne!=0) # assume 'jamne' is Sel_sel and change these
community$Sel_sel[c(8944,8964)] <- 1

# there is a lot of details to clean, like _cf species etc. Most of these will likely be removed later when deleting 'rare species' in S0 script

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
# same for whole data set
community <- community[!community$year==2018,]
community <- rbind(community18,community)

# keep only relevant columns
community18 <- community18 %>%
  select(Site,blockID,plotID,subPlotID,
         Ach_mil:Vio_sp)

# save clean data
#------------------------------------
write.csv(community,'Data/Community/INCLINE_community_2018_2019_2021_clean.csv')
write.csv(community18,'Data/Community/INCLINE_community_2018_clean.csv')


#################### FunCab validation data ##########################

# Another project at the Vestland Climate Grid, FunCab, has community data 
# in different blocks and plots in the same sites. 

# download raw data from FunCab OSF (https://osf.io/dutkw/) to data folder
funcab <- read.csv('Data/Community/FunCaB_raw_community_2015-2019/raw/funcab_composition_2015-utenGud.csv', sep = ';')
gudmedalen <- read.csv ('Data/Community/FunCaB_raw_community_2015-2019/funcab_Gudmedalen.csv')
funcab[1:10,1:10]
gudmedalen[1:10,1:10]

# make the two data frames same format before merging them
funcab <- funcab %>%
  select(siteID = ï..siteID,blockID,turfID,subPlot,Ach.mil:Vis.vul) %>%
  mutate_if(is.integer, as.character) %>%
  mutate_if(is.logical, as.character)
names(funcab) <- gsub('[.]','_',names(funcab))
funcab <- subset(funcab, subset = siteID %in% c('Skjellingahaugen','Lavisdalen','Ulvhaugen'))

gudmedalen <- gudmedalen %>%
  select(siteID,blockID,turfID,subPlot,Ach_mil:Vis_vul) %>%
  mutate_if(is.integer, as.character)

funcab <- bind_rows(funcab,gudmedalen)
names(funcab) # examine names to see if more cleaning is needed...

# convert to PA
funcab <- funcab %>% 
  mutate(across(c(Ach_mil:Vis_vul), ~ifelse(.=="", 0, as.character(.)))) %>% # change blanks to 0
  mutate(across(c(Ach_mil:Vis_vul), ~ifelse(is.na(.), 0, as.character(.)))) %>% # change NAs to 0
  mutate(across(c(Ach_mil:Vis_vul), ~ifelse(.!="0", 1, as.character(.)))) # change the remaining values (F,S,J etc) to 1 (present)
# the 'else' in the functions returns the value as it was, in character format.

length(unique(funcab$turfID)) # how many plots are we left with?

# change species names from Gen.spe to Gen_spe
funcab$species <- gsub('[.]','_',funcab$species)

# what are the 'T' subplots?


# save changes
#------------------------
write.csv(funcab,'Data/Community/FunCaB_subplot_composition.csv')
