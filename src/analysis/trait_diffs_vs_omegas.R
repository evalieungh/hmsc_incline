##############################################
#  pairwise trait differences versus omegas  #
##############################################

# script by Eva Lieungh, Ryan Burner
# started 2023-02-08

library(Hmsc)
library(ape)
library(reshape2)
library(ggplot2)

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/")

# read in data
omegas_long <- readRDS("../../data_processed/omegas_long.Rds")
head(omegas_long)
traits <- readRDS("../../data_processed/traits_wide_all.RData")
head(traits)

# change spelling of Skjellingahaugen to get matching site names
traits$siteID[traits$siteID == "Skjelingahaugen"] <-
  "Skjellingahaugen"
identical(unique(omegas_long$siteID), unique(traits$siteID))

# check which species we can compare
unique(omegas_long$speciesA) # PROBLEM this starts with Agr_mer, while speciesB starts with Ach_mil. Final species Vio_pal/Vio_bif !!
unique(traits$species)
species = unique(omegas_long$speciesA[omegas_long$speciesA %in% traits$species])
species

# 
