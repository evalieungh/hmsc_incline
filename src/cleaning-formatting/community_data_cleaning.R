#####################################
#  Download and clean INCLINE data  #
#####################################

# Script by Eva Lieungh
# started 2023-03-06

# Clean INCLINE community data from OSF,
# 
# 

library(tidyverse)

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/cleaning-formatting/")

community <- 
  read.csv("../../data/VCG/INCLINE_community/community_clean_med_NA_2023-03-06.csv", 
                      sep = ";")
head(community)

# one row has NA instead of 0/1. Change to 0
community[community$plotID== "Gud_7_4" & community$subPlot == "2" & community$species == "Ran_acr","presence"] <- 0

# subset only 2018 data
community <- subset(community,year==2018)

# add unique block ID and subplot ID
community$blockID <- substr(community$plotID,
                            start = 1,
                            stop = 5)
community$subPlotID <- paste(community$plotID, 
                             community$subPlot,
                             sep = "_")

# found out subPlotID Skj_7_1_23 is there, but should not be.
row_to_delete = which(community$subPlotID == "Skj_7_1_23")
community <- community[-row_to_delete,]

# Check for duplicated rows
duplicated_rows <- duplicated(community)

# Identify which rows are duplicated
duplicated_indices <- which(duplicated_rows)

# Print the duplicated rows
if (length(duplicated_indices) > 0) {
  cat("The following rows are duplicated:\n")
  print(community[duplicated_indices, ])
} else {
  cat("No duplicated rows found.\n")
}

# remove duplicates
community <- unique(community)

# pivot wider
community <- community %>%
  pivot_wider(names_from = species, 
              values_from = presence)

# add new unique subplot ID
community$subPlotID = paste(substr(community$site,1,3),
                            community$block,
                            community$plot,
                            community$subPlot,
                            sep = '_')

# subset only 2018 data
community <- subset(community,year==2018)

# and columns with study design and species
community <- community %>%
  select(site, block, plot, subPlotID,
         everything())


# save clean data
write.csv(community,"../../data/VCG/INCLINE_community/INCLINE_community_2018_clean.csv")
