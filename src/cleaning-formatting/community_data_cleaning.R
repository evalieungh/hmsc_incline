#####################################
#  Download and clean INCLINE data  #
#####################################

# Script by Eva Lieungh
# started 2023-03-06

# Clean INCLINE community data from OSF,
# https://osf.io/mn3t9
# INCLINE_community_subplot.csv
# downloaded 2023-03-08

library(tidyverse)

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")

community <- 
  read.csv("data/VCG/INCLINE_community/INCLINE_community_subplot.csv", 
                      sep = ";")
head(community)

# one row has NA instead of 0/1. Change to 0
community[community$plotID == "Gud_7_4" &
            community$subPlot == "2" &
            community$species == "Ran_acr", "presence"] <- 0

# subset only 2018 data
community <- subset(community,year==2018)

# add unique block ID and subplot ID
community$blockID <- substr(community$plotID,
                            start = 1,
                            stop = 5)
community$subPlotID <- paste(community$plotID, 
                             community$subPlot,
                             sep = "_")

# remove duplicates
community <- unique(community)

# select only relevant columns, and 
# pivot wider to get subplots as rows, species as columns
community <- community %>%
  select(site, blockID, plotID, subPlotID,  
         logger, vegetation_height_mm, moss_depth_mm,
         species, presence) %>%
  pivot_wider(names_from = species, 
              values_from = presence, 
              values_fn = function(x) max(x),
              values_fill = 0)

community[1:5,1:10]

# Check if any species should be left out
names(community)
remove_these_species = c("Nid_seedling", "Unknown", "Fern")

# remove uninformative taxa
community <- community[, -which(names(community) %in% remove_these_species)]

# save clean data
write.csv(community,
          "data/VCG/INCLINE_community/INCLINE_community_2018_clean.csv",
          row.names = FALSE)

