###############################################
#  plot relationships between species traits  # 
#      and pairwise co-occurrence scores      #
###############################################

# Script by Eva Lieungh, Ryan Burner
# started 2023-01-26

# make a plot, similar to Fig 4 in Burner et al. 2021
# https://onlinelibrary.wiley.com/doi/epdf/10.1111/jbi.14272, 
# https://onlinelibrary.wiley.com/cms/asset/4a44d384-37dc-426b-b162-3b2684f3537f/jbi14272-fig-0004-m.png
# as a collection of plots per trait, with site on y axis,
# and relationships between species traits and pairwise co-occurrence scores
# as bars with confidence intervals along x axis.

library(tidyverse)
library(ggplot2)

# 1. read and format data
# --------------------------------
# In Ryan's example, he first formatted the data to get a data frame
# with column setup like this:
# speciesA, speciesB, omega, support [posterior probability], 
# positiveOmega [binary, 1 if omega has support over threshold value],
# negativeOmega, spA_trait1, spA_trait2, spB_trait1, spB_trait2 etc...
setwd(
  'C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/'
)

# read in data
omegas <- read.csv("data_processed/omegas_long_support.csv")
head(omegas)
unique(omegas$siteID)

traits <- readRDS("data_processed/traits_wide_all.RData")
head(traits)
unique(traits$siteID)

# do they contain same species?
length(unique(omegas$speciesA))
length(unique(traits$species))

not_in_either <-
  setdiff(union(unique(omegas$speciesA), unique(traits$species)),
          intersect(unique(omegas$speciesA), unique(traits$species)))
print(not_in_either) # 21 species not present in both data sets
  # (i.e. missing from either traits or omegas data set)

# subset both data sets so they only contain the same species
omegas <- omegas[!omegas$speciesA %in% not_in_either &
                   !omegas$speciesB %in% not_in_either, ]

traits <- traits[!traits$species %in% not_in_either, ]

# Create new columns with binary support/not for positive and negative values
omegas$positiveOmega <- ifelse(omegas$support_positive > 0.90, 1, 0)
omegas$negativeOmega <- ifelse(omegas$support_negative > 0.90, 1, 0)

# select traits, collapse to species, fill NAs with averages
table(traits$siteID, traits$species) # site-specific measurements, 
  # but many species were not measured at all sites! Need to fill in the
  # missing values with averages (or max?) from the other observations.

names(traits) 
# height:leaf_thickness have many observations but some NAs, 
# the rest of the nutrient traits have much fewer observations. 

# collapse trait values to max for each species and site. 
# Choosing max instead of mean now because it might better reflect the species'
# *potential* than the mean does.
traits_species <- traits %>%
  group_by(species, siteID) %>%
  summarise(
    height_mm = max(height_mm, na.rm = TRUE),
    fresh_mass_g = max(fresh_mass_g, na.rm = TRUE),
    dry_mass_g = max(dry_mass_g, na.rm = TRUE),
    leaf_area_cm2 = max(leaf_area_cm2, na.rm = TRUE),
    SLA_cm2_g = max(SLA_cm2_g, na.rm = TRUE),
    LDMC_g_g = max(LDMC_g_g, na.rm = TRUE),
    leaf_thickness = max(leaf_thickness, na.rm = TRUE),
    N_percent = max(N_percent, na.rm = TRUE),
    C_percent = max(C_percent, na.rm = TRUE),
    CN_ratio = max(CN_ratio, na.rm = TRUE)) %>% # warnings: created -Inf for missing values.  
  mutate_all(~replace(., . == -Inf, NA)) %>% # Replace with NAs.
  mutate(across('siteID', # fix spelling difference in one site name
                str_replace, 
                'Skjelingahaugen', 
                'Skjellingahaugen'))
head(traits_species)
colnames(traits_species)[colnames(traits_species) == "species"] <- "speciesA"

# combine omega and trait data
data <- omegas %>%
  left_join(traits_species,
            by = c("siteID", "speciesA"))
names(data)

# add trait data for speciesB
traits = c(
  "height_mm",
  "fresh_mass_g",
  "dry_mass_g",
  "leaf_area_cm2",
  "SLA_cm2_g",
  "LDMC_g_g",
  "leaf_thickness",
  "N_percent",
  "C_percent",
  "CN_ratio"
)

for (trait in traits) {
  traitname <- paste(trait, "B", sep = "_")
  data[[traitname]] <-
    data[[trait]][match(data$speciesB, data$speciesA)] #  find the indices of speciesB in speciesA, subset corresponding value
}

# 2. calculate differences in traits between each pair of species
# ------------------------------------------------------------------
# here we will calculate trait differences for each trait and species pair.
# Log transform diff=log(trait)-log(trait) to reduce marginal impact of huge differences
(start_trait = which(colnames(data) == "height_mm")) # first column with trait
no_of_traits = length(traits)
progressbar = txtProgressBar(min = 0,
                             max = length(data[, 1]),
                             initial = 0)
a = Sys.time()
for (i in 1:length(data[, 1])) {
  for (j in 1:no_of_traits) {
    data[i, (start_trait - 1) + j] = m$TrData[which(rownames(m$TrData) == MeltNew$SpA[i]), trFind[j]]
    MeltNew[i, (startTrait - 1) + length(trFind) + j] = m$TrData[which(rownames(m$TrData) ==
                                                                         MeltNew$SpB[i]), trFind[j]]
    MeltNew[i, (startTrait - 1) + (length(trFind) * 2) + j] = MeltNew[i, (startTrait -
                                                                            1) + j] - MeltNew[i, (startTrait - 1) + length(trFind) + j]
  }
  setTxtProgressBar(progressbar, i) # track progress visually
}
Sys.time()-a
