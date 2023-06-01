################################
#  format omega output matrix  #
################################

# script by Eva Lieungh

library(tidyverse)

# Omegas stored from OO's Hmsc pipeline as .xlsx with co-occurrences, 
# posterior probabilities of omega>0 and <0 (sum to 1). 
# I manually saved the omegas and positive/negative probabilities as .csv files.

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")

# read data matrix (a=association=omega estimate)
#-------------------------------------
a_skj <- read.csv('results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Skjellingahaugen_subplot_mean.csv',
                  row.names = 1, sep = ";", dec = ",")
a_gud <- read.csv('results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Gudmedalen_subplot_mean.csv',
                  row.names = 1, sep = ";", dec = ",") # estimated associations
a_lav <- read.csv('results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Lavisdalen_subplot_mean.csv',
                  row.names = 1, sep = ";", dec = ",")
a_ulv <- read.csv('results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Ulvehaugen_subplot_mean.csv',
                  row.names = 1, sep = ";", dec = ",")

# collect all data as list of association objects
a_obj = list(
  'a_skj' = a_skj,
  'a_gud' = a_gud,
  'a_lav' = a_lav,
  'a_ulv' = a_ulv
)

# save a_obj list of dataframes for use in other scripts, full matrices
saveRDS(a_obj,file = 'data_processed/a_obj_full.Rds')

# set upper triangle of matrices to NA, including the diagonal
for (i in 1:4) {
  a_obj[[i]][upper.tri(as.matrix(a_obj[[i]]), diag=TRUE)] <- NA
}
length(which(is.na(a_obj[[3]]))) # number of NAs in half matrix should be (55*54)/2+55 = 1540

# save a_obj list of dataframes for use in other scripts, lower half of matrix
saveRDS(a_obj,'data_processed/a_obj_lower.Rds')

# pivot longer 
# --------------------------------------
# ...to get species pairs as columns and their co-occurrence as third axis
a_obj <- readRDS('data_processed/a_obj_lower.Rds')

# collect all omega matrices for all 4 sites
omegas <- data.frame(siteID = c(rep('Skjellingahaugen',55),
                                rep('Gudmedalen',55),
                                rep('Lavisdalen',55),
                                rep('Ulvehaugen',55)),
                     speciesA = rep(rownames(a_obj[[1]]),4),
                     rbind(a_obj[[1]],a_obj[[2]],a_obj[[3]],a_obj[[4]]))

# pivot longer to get sites, species pairs (A and B), and omega value
omegas_l <- omegas %>%
  pivot_longer(Ach_mil:Vio_pal, # pivot all species, but not site or species name columns
               names_to = 'speciesB',
               values_to = 'omega')

# remove same species rows
omegas_l <- omegas_l[omegas_l$speciesA != omegas_l$speciesB,]

# remove NAs (original upper triangle)
omegas_l <- omegas_l[!is.na(omegas_l$omega),]
  
# save object
saveRDS(omegas_l,'data_processed/omegas_long.Rds')

# add posterior support columns
# ------------------------------------------------
# i.e. add posterior probability, 
# positive = posterior probability that omega > 0, and vice versa
omegas_l <- readRDS("data_processed/omegas_long.Rds")

omegas_support_positive <- list(
  skj_pos = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Skjellingahaugen_subplot_positive.csv",
                     row.names = 1, sep = ";", dec = ","),
  gud_pos = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Gudmedalen_subplot_posteriors_positive.csv",
                     row.names = 1, sep = ";", dec = ","),
  lav_pos = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Lavisdalen_subplot_positive.csv",
                     row.names = 1, sep = ";", dec = ","),
  ulv_pos = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Ulvehaugen_subplot_positive.csv",
                     row.names = 1, sep = ";", dec = ",")
)

omegas_support_negative <- list(
  skj_neg = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Skjellingahaugen_subplot_negative.csv",
                     row.names = 1, sep = ";", dec = ","),
  gud_neg = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Gudmedalen_subplot_posteriors_negative.csv",
                     row.names = 1, sep = ";", dec = ","),
  lav_neg = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Lavisdalen_subplot_negative.csv",
                     row.names = 1, sep = ";", dec = ","),
  ulv_neg = read.csv("results/OO_2023-03-22_scripts_and_results/results/parameter_estimates_Omega_Ulvehaugen_subplot_negative.csv",
                     row.names = 1, sep = ";", dec = ",")
)

# set upper triangles and diagonals to NA
for (i in 1:4) {
  omegas_support_positive[[i]][upper.tri(as.matrix(omegas_support_positive[[i]]),
                                         diag = TRUE)] <- NA
  omegas_support_negative[[i]][upper.tri(as.matrix(omegas_support_negative[[i]]),
                                         diag = TRUE)] <- NA
}
length(which(is.na(omegas_support_positive[[3]]))) # number of NAs in half matrix should be (55*54)/2+55 = 1540
length(which(is.na(omegas_support_negative[[2]])))

# collect all matrices for all 4 sites
supports_positive <-
  data.frame(
    siteID = c(
      rep('Skjellingahaugen', 55),
      rep('Gudmedalen', 55),
      rep('Lavisdalen', 55),
      rep('Ulvehaugen', 55)
    ),
    speciesA = rep(rownames(omegas_support_positive[[1]]), 4),
    rbind(
      omegas_support_positive[[1]],
      omegas_support_positive[[2]],
      omegas_support_positive[[3]],
      omegas_support_positive[[4]]
    )
  )

supports_negative <-
  data.frame(
    siteID = c(
      rep('Skjellingahaugen', 55),
      rep('Gudmedalen', 55),
      rep('Lavisdalen', 55),
      rep('Ulvehaugen', 55)
    ),
    speciesA = rep(rownames(omegas_support_negative[[1]]), 4),
    rbind(
      omegas_support_negative[[1]],
      omegas_support_negative[[2]],
      omegas_support_negative[[3]],
      omegas_support_negative[[4]]
    )
  )

# pivot longer to get sites, species pairs (A and B), and omega support value
supports_positive_long <- supports_positive %>%
  pivot_longer(Ach_mil:Vio_pal, # pivot all species, but not site or species name columns
               names_to = 'speciesB',
               values_to = 'support_positive')
head(supports_positive_long)
tail(supports_positive_long)

supports_negative_long <- supports_negative %>%
  pivot_longer(Ach_mil:Vio_pal, # pivot all species, but not site or species name columns
               names_to = 'speciesB',
               values_to = 'support_negative')

# merge data for omega estimates (posterior means), positive support and negative support
omegas_long_support <- omegas_l %>%
  left_join(supports_positive_long, by = c("siteID", "speciesA", "speciesB")) %>%
  left_join(supports_negative_long, by = c("siteID", "speciesA", "speciesB"))

write.csv(omegas_long_support, 
          "data_processed/omegas_long_support.csv", 
          row.names = FALSE)

# sum co-occurrences per species and pivot long
# ------------------------------------------------
# read if necessary
Mfull <- readRDS('../../data_processed/a_obj_full.RData')

# test with only Skjellingahaugen omegas first:
# remove diagonal (set to NA)
diag(a_skj)=0

# Add column with sums of rows (species summed assoc.)
M_sums <- cbind(a_skj,'summed' = c(rowSums(na.omit(a_skj))))

# sort whole matrix after sum
M_sort <- M_sums[order(M_sums[,'summed'],decreasing=TRUE),]

# save reversed order of species sorted by their summed co-occurrences
sp_order_sum <- rev(c(rownames(M_sort)))

# row names to column, convert matrix to dataframe
df <- rownames_to_column(as.data.frame(M_sort),'sp1')

# pivot longer
df <- df %>%
  pivot_longer(Ach_mil:Vio_pal, names_to = 'sp2', values_to = 'association')

# remove identical species pairs (diagonal of matrix)
df <- df[- which(df$sp1==df$sp2),]

# convert species columns from character to factors
df$sp1 <- as.factor(df$sp1)
df$sp2 <- as.factor(df$sp2)

# compute mean per species
df <- df %>%
  group_by(sp1) %>%
  mutate(means = mean(association))

# make new column with |.33| as cutoff for positive, weak, and negative co-occurrence associations
df <- df %>%
  mutate(assoc_strength = ifelse(association > 0.33, 'pos',
                                 ifelse(association < -0.33, 'neg', 'weak')))
# save object
saveRDS(df,'data_processed/omegas_species_sums.RData')


# subsetting
# -----------------------------------
# subset 'friends' and 'enemies', 
# saved from the S10_omegas_stats script
friend_rows <- read.csv('data/friend_rows.csv')
enemy_rows <- read.csv('data/enemy_rows.csv')

# subset selected species
