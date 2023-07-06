##################################
#  format omega and trait data,  #
#  make omega ~ traits models,   #
#  for figure 3                  #
##################################

# Script by Eva Lieungh
# started 2023-01-26

# inspired by unpublished script by Ryan Burner

# this script creates data files:
# data_processed/fig3_omegas_traits_data.csv
# data_processed/fig3_omega-trait_model_data.Rds

library(tidyverse)
library(ggplot2)

# read and format data
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
# but many species were not measured at all sites! Might need to fill in the
# missing values with values (average, max?) from the other observations.

names(traits) 
# height:leaf_thickness have many observations but some NAs, 
# the rest of the nutrient traits have much fewer observations. 

# collapse trait values to max for each species and site. 
# Choosing max instead of mean now because it might better reflect the species'
# *potential* than the mean does.
# treat these as species A so we can add species B later for comparison.
# Lots of warnings because not all species have trait data -> creates NAs
traits_species <- traits %>%
  group_by(species, siteID) %>%
  summarise(
    spA_height_mm = max(height_mm, na.rm = TRUE),
    spA_fresh_mass_g = max(fresh_mass_g, na.rm = TRUE),
    spA_dry_mass_g = max(dry_mass_g, na.rm = TRUE),
    spA_leaf_area_cm2 = max(leaf_area_cm2, na.rm = TRUE),
    spA_SLA_cm2_g = max(SLA_cm2_g, na.rm = TRUE),
    spA_LDMC_g_g = max(LDMC_g_g, na.rm = TRUE),
    spA_leaf_thickness = max(leaf_thickness, na.rm = TRUE),
    spA_N_percent = max(N_percent, na.rm = TRUE),
    spA_C_percent = max(C_percent, na.rm = TRUE),
    spA_CN_ratio = max(CN_ratio, na.rm = TRUE)) %>% # warnings: created -Inf for missing values.  
  mutate_all(~replace(., . == -Inf, NA)) %>% # Replace with NAs.
  mutate(across('siteID', # fix spelling difference in one site name
                str_replace, 
                'Skjelingahaugen', 
                'Skjellingahaugen'))
colnames(traits_species)[colnames(traits_species) == "species"] <- "speciesA"
head(traits_species)
# combine omega and trait data
data <- omegas %>%
  left_join(traits_species,
            by = c("siteID", "speciesA"))
names(data)

# add trait data for speciesB
traitlist = c(
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

for (trait in traitlist) {
  spB_traitname = paste("spB", trait, sep = "_") # new column to be generated
  spA_traitname = paste("spA", trait, sep = "_") # existing trait column to look far value in
  #  find the indices of speciesB in speciesA, subset corresponding value
  data[[spB_traitname]] <-
    data[[spA_traitname]][match(data$speciesB, data$speciesA)] 
}
head(data)

# calculate differences in traits between each pair of species
# ------------------------------------------------------------------
# calculate trait differences for each trait and species pair.
# All traits are numerical.
# Log transform diff = log(trait) - log(trait) 
# to reduce marginal impact of huge differences
for (trait in traitlist) {
  spB_traitname = paste("spB", trait, sep = "_")
  spA_traitname = paste("spA", trait, sep = "_")
  traitname = paste(trait, "diff", sep = "_") # new column to be made
  data[[traitname]] <- 
    log(data[[spB_traitname]]) - log(data[[spA_traitname]])
}

# add in occupancy for each species
# --------------------------------------------
SXY <- read.csv("data/SXY.csv")
head(SXY)
Y <- SXY %>% select(Ach_mil:Vio_pal)
(occupancies = colSums(Y))

data$occSpA=-999
data$occSpB=-999

for (i in 1:nrow(data)) {
  data$occSpA[i] =
    occupancies[which(names(occupancies) == data$speciesA[i])] / nrow(Y)
  data$occSpB[i] =
    occupancies[which(names(occupancies) == data$speciesB[i])] / nrow(Y)
}

write.csv(data, 
          "data_processed/fig3_omegas_traits_data.csv",
          row.names = FALSE)

# make glms of omegas modelled by trait differences
# ---------------------------------------------------

# make vector of all trait difference columns to use as covariates in model
(trait_diff_vector = colnames(data)[grep("_diff", colnames(data))])

# repeat for each site (4 sites)
(trait_diff_vector <- c(rep(trait_diff_vector, each = 4)))

# to get cleaner names in the plot, make corresponding 
# new vectors of traits, sites
(traitlist_plot = c(rep(
  c(
    "Height",
    "Fresh mass",
    "Dry mass",
    "Leaf area",
    "SLA",
    "LDMC",
    "Leaf thickness",
    "N percent",
    "C percent",
    "CN ratio"
  ),
  each = 4
)))

(sitelist_plot = c(rep(
  c("Skjellingahaugen",
    "Gudmedalen",
    "Lavisdalen",
    "Ulvehaugen"),
  length(traitlist)
)))

#make lists for model output
modelOutput=list()
modelCoeffs=list()

str(data)

# run model for site-trait combination
i = 1
for (i in 1:length(trait_diff_vector)) {
  # create generalised linear models for site-trait combinations
  modelOutput[[i]] = glm(formula(paste0("omega ~ ", trait_diff_vector[i])),
                         data = data[which(data$siteID == sitelist_plot[i]),],
                         family = gaussian)
  # round model coefficients
  modelCoeffs[[i]] = round(summary(modelOutput[[i]])$coefficients, 4)
  # add estimated trait difference (Transf_est) and other info to output
  modelCoeffs[[i]] = cbind(modelCoeffs[[i]], modelCoeffs[[i]][, 1])
  modelCoeffs[[i]][, length(modelCoeffs[[i]][1,])] =
    round(as.numeric(exp(modelCoeffs[[i]][, 1])), 3)
  colnames(modelCoeffs[[i]])[length(colnames(modelCoeffs[[i]]))] = "Transf_est"
  names(modelOutput)[i] = paste0(traitlist_plot[i], "_", sitelist_plot[i])
  names(modelCoeffs)[i] = paste0(traitlist_plot[i], "_", sitelist_plot[i])
  print(paste0(round(i / length(
    trait_diff_vector
  ), 3) * 100, "%"))
}

#view coefficients
modelCoeffs

# Are these relationships between traits and co-occurrences similar among sites?
# ------------------------------------------------------------------------------

# format model output for plotting
model_data = melt(modelCoeffs)
model_estimate = model_data[which(model_data$Var2 == "Estimate"), ]
model_SE = model_data[which(model_data$Var2 == "Std. Error"), ]
model_data = cbind(model_estimate, model_SE)
head(model_data)
tail(model_data)

# select variable, value, trait-site, SE-value columns
model_data = model_data[,c(1,3,4,7)]
head(model_data)

sub('.*>>', '', x)
# rename some columns
model_data$siteID = sub(".*_", "", model_data$L1)
names(model_data)[2]="Estimate"
names(model_data)[4]="SE"
head(model_data)

# get confidence intervals
model_data$min95 = model_data$Estimate - (1.96 * model_data$SE) 
model_data$max95 = model_data$Estimate + (1.96 * model_data$SE) 
model_data$min90 = model_data$Estimate - (1.645 * model_data$SE)
model_data$max90 = model_data$Estimate + (1.645 * model_data$SE)
model_data$Coefficient = model_data$Estimate
model_data$Estimate = exp(model_data$Estimate)
head(model_data)

# label each coefficient as significant or not
# at 90% and 95%; color them accordingly for plotting
model_data$Sig95 = "grey" # set grey as placeholder in entire column
for (i in 1:length(model_data$Sig95)) { # for each row,
  # if the sign (+/0/-) is the same inside 95% CI,
  if (sign(model_data$min95[i]) == sign(model_data$max95[i])) {
    # and if that sign is positive (sign() gives 1),
    if (sign(model_data$min95[i]) == 1) {
      # set color label red.
      model_data$Sig95[i] = "red"
    }
    # If the sign is negative, set blue label.
    else {
      model_data$Sig95[i] = "blue"
    }
  }
}

model_data$Sig90 = "grey"
for (i in 1:length(model_data$Sig90)) {
  if (sign(model_data$min90[i]) == sign(model_data$max90[i])) {
    if (sign(model_data$min90[i]) == 1) {
      model_data$Sig90[i] = "red"
    }
    else {
      model_data$Sig90[i] = "blue"
    }
  }
}

model_data[1:12,]

# save model data for plotting
saveRDS(model_data, "data_processed/fig3_omega-trait_model_data.Rds")





