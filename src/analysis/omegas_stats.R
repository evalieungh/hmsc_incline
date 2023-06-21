###################################
#    summarise omega estimates    #
###################################

# script by EL

# prerequisite: format_omegas.R script, which formats the original output matrix a bit.

library(tidyverse)

# read in data
a_obj_full <- readRDS('data/a_obj_full.RData')
a_obj <- readRDS('data/a_obj_lower.RData')

# summarise positive and negative associations
#---------------------------------------
# total number of positive and negative association estimates, per site
for (i in 1:4) {
  print(length(which(a_obj[[i]] > 0))) # positive
}
for (i in 1:4) {
  print(length(which(a_obj[[i]] < 0))) #negative
}
plot(c(1:4),c(809,836,850,827)) # plot +
plot(c(1:4),c(844,817,803,826)) # -
sum(na.omit(a_obj[[4]])) # whole matrix sums to 0

# find max and min value for each species pair across sites
max_a <- pmax(a_obj[[1]],a_obj[[2]],a_obj[[3]],a_obj[[4]])
max_a[2:5, 1:5]
min_a <- pmin(a_obj[[1]],a_obj[[2]],a_obj[[3]],a_obj[[4]])
min_a[2:5, 1:5] # all of these (e.g. Ach_mil--Agr_cap) have both positive and negative associations somewhere!

# how many, and which, species pairs have 
# negative/positive co-occurrences in all 4 sites?
# -----------------------------------------

# the null model assuming random co-occurrence would be:
1485 / 2^4 # species pairs / chance of being positive
  
# how many species pairs have positive associations in at least one site?
length(which(max_a > 0)) # 1410 positive in at least one site
length(which(min_a < 0)) # 1375 negative in at least one site

# how many species pairs always have positive/negative associations in all sites?
length(which(min_a > 0)) # 110 positive -> "friends"
length(which(max_a < 0)) # 75 negative -> "enemies"

# save 'friends' and 'enemies'
# Create an empty data frame to store the results
friend_pairs <- data.frame(
  speciesA = character(),
  speciesB = character(),
  stringsAsFactors = FALSE
)
enemy_pairs <- data.frame(
  speciesA = character(),
  speciesB = character(),
  stringsAsFactors = FALSE
)

# Iterate over each row and column index to fill in species pairs
for (i in 1:55) { # "friends"
  for (j in 1:55) {
    value <- min_a[i, j]
    if (!is.na(value) && value > 0) {
      speciesA <- rownames(min_a)[i]
      speciesB <- rownames(min_a)[j]
      friend_pairs <- rbind(
        friend_pairs,
        data.frame(
          speciesA,
          speciesB,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}
for (i in 1:55) { # "enemies"
  for (j in 1:55) {
    value <- max_a[i, j]
    if (!is.na(value) && value < 0) {
      speciesA <- rownames(max_a)[i]
      speciesB <- rownames(max_a)[j]
      enemy_pairs <- rbind(
        enemy_pairs,
        data.frame(
          speciesA,
          speciesB,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

# add omega values for each site
sites = c("a_skj", "a_gud", "a_lav", "a_ulv")
for (site in sites) { # "friends"
  # Create a new column for the site
  friend_pairs[, site] <- NA
  # Iterate over each row in 'friend_pairs'
  for (i in 1:nrow(friend_pairs)) {
    speciesA <- friend_pairs$speciesA[i]
    speciesB <- friend_pairs$speciesB[i]
    
    # Find the corresponding value based on species names
    value <- a_obj[[site]][speciesA, speciesB]
    
    # Assign the value
    friend_pairs[i, site] <- value
  }
}
for (site in sites) { # "enemies"
  # Create a new column for the site
  enemy_pairs[, site] <- NA
  # Iterate over each row
  for (i in 1:nrow(enemy_pairs)) {
    speciesA <- enemy_pairs$speciesA[i]
    speciesB <- enemy_pairs$speciesB[i]
    
    # Find the corresponding value based on species names
    value <- a_obj[[site]][speciesA, speciesB]
    
    # Assign the value
    enemy_pairs[i, site] <- value
  }
}

write.csv(friend_pairs,
          'results/friend_pairs.csv',
          row.names = FALSE)
write.csv(enemy_pairs,
          'results/enemy_pairs.csv',
          row.names = FALSE)

# strength of co-occurrences
#--------------------------------------------
# plot histograms per site
par(mfrow=c(2,2))
for (i in 1:4) {
 hist(as.matrix(a_obj[[i]][,2:ncol(a_obj[[i]])]))
} # looks like Gudmedalen (?) sticks out with a wider distribution of associations






