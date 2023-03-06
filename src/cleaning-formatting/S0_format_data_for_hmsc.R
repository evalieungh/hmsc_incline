####################################
# Data formatting for HMSC scripts #
####################################

# script by Eva Lieungh
# started 2020

# Create an SXY file to comply with Hmsc scripts: 
# study design (S),
# covariates (X), and 
# species occurrences (Y)

library(tidyverse)

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/cleaning-formatting/")

# Study design and community data (subplot presence/absence)
#------------------------------------------
# start SXY object to be filled with more data
SXY <- read.csv('../../data/VCG/INCLINE_community/INCLINE_community_2018_clean.csv') # presence-absence S (study design) and Y (species) data from 2018
SXY[1:3,1:10]

# make precipitation covariate, annual means
SXY <- SXY %>%
  mutate(prec = as.integer(ifelse(
    Site == 'Skjellingahaugen', 3402,
    ifelse(
      Site == 'Gudmedalen', 2130,
      ifelse(
        Site == 'Lavisdalen', 1561,
        ifelse(
          Site == 'Ulvehaugen', 1226, (.)))
    )
  )))

# reorder columns
SXY <- SXY %>%
  select(Site:subPlotID,prec,Ach_mil:Vio_sp)
SXY[1:5,1:10]

# remove too rare or common species
Y = SXY[,6:ncol(SXY)] # extract species data
ubiquitous = round(nrow(Y) * 0.9) # define threshold for being too common
rare = round(nrow(Y) * 0.01) # define threshold for being too rare
remove_pa = which(colSums(Y>0)<rare|colSums(Y>0)>ubiquitous)
length(remove_pa) # how many species are too rare, or ubiquitous?
Y <- Y[,-remove_pa] # remove those species

colnames(Y)[grepl('_cf',colnames(Y))] # check for uncertain species
colnames(Y)[grepl('_sp',colnames(Y))] # check for genus-level records. Include of exclude?

# final list of species to include
species <- c(names(Y))
write.csv(species,'Data/specieslist.csv',row.names = FALSE)

# save SXY with the correct species
SXY <- data.frame(SXY[,1:5],Y) 


# Microclimatic covariates (x)
#------------------------------------------
# soil moisture and temperature.
microclimate_data_list <- 
  readRDS('../../data_processed/subplot_data_sitespecific_list.Rds')
microclimate_data_list[["skj"]][1:5,1:8]


# export SXY file
write.csv(SXY,'Data/SXY.csv',row.names = FALSE)
