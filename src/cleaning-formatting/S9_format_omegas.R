###################################
#    format omega output matrix   #
###################################

# script by Eva Lieungh

library(tidyverse)

# Omegas stored from OO's Hmsc pipeline as .xlsx with associations, posterior probabilities of omega>0 and <0 (sum to 1). Manually saved the omegas and positive probabilities as .csv files. 

# read data matrix (a=association=omega estimate)
#-------------------------------------
a_skj <- read.csv('data/model_output/Omega_Skjellingahaugen_subplot.csv', row.names = 1)
a_gud <- read.csv('data/model_output/Omega_Gudmedalen_subplot.csv',row.names = 1) # estimated associations
a_lav <- read.csv('data/model_output/Omega_Lavisdalen_subplot.csv',row.names = 1)
a_ulv <- read.csv('data/model_output/Omega_Ulvehaugen_subplot.csv',row.names = 1)

# collect all data as list of association objects
a_obj = list('a_skj'=a_skj,
             'a_gud'=a_gud,
             'a_lav'=a_lav,
             'a_ulv'=a_ulv)

# save a_obj list of dataframes for use in other scripts, full matrices
saveRDS(a_obj,file = 'data/a_obj_full.RData')

# set upper triangle of matrices to NA, including the diagonal
for (i in 1:4) {
  a_obj[[i]][upper.tri(as.matrix(a_obj[[i]]), diag=TRUE)] <- NA
}
length(which(is.na(a_obj[[3]]))) # number of NAs in half matrix should be (58*57)/2+58=1711

# save a_obj list of dataframes for use in other scripts, lower half of matrix
saveRDS(a_obj,file = 'data/a_obj_lower.RData')

# format for plotting
# -----------------------------------
# read if necessary
Mfull <- readRDS('data/a_obj_full.RData')

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
saveRDS(df,'data/plotting_df.RData')

# subsetting
# -----------------------------------
# subset 'friends' and 'enemies', 
# saved from the S10_omegas_stats script
friend_rows <- read.csv('data/friend_rows.csv')
enemy_rows <- read.csv('data/enemy_rows.csv')

# subset selected species