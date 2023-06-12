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

# how many, and which, species pairs have negative/positive co-occurrences in all 4 sites?
# the null model assuming random co-occurrence would be:
(58 * 58) /2^4 # species pairs / chance of being positive
  
# how many species pairs have positive associations in at least one site?
length(which(max_a > 0)) # 1545 positive in at least one site
length(which(min_a < 0)) # 1537 negative in at least one site

# how many species pairs always have positive/negative associations in all sites?
length(which(min_a > 0)) # 116 positive -> "friends"
length(which(max_a < 0)) # 108 negative -> "enemies"

# save 'friends' and 'enemies'
friend_rows <- as.data.frame(which(min_a > 0, arr.ind = TRUE))
friend_rows$species <- rownames(friend_rows)
friend_rows <- friend_rows %>% 
  select(species, sp1_row = row, sp2_col = col) %>%
  arrange(sp1_row)
friend_rows$species <- gsub('\\..*','', friend_rows$species) # remove .1 etc that got added to replicated species names

enemy_rows <- as.data.frame(which(max_a < 0, arr.ind = TRUE))
enemy_rows$species <- rownames(enemy_rows)
enemy_rows <- enemy_rows %>% 
  select(species, sp1_row = row, sp2_col = col) %>%
  arrange(sp1_row)
enemy_rows$species <- gsub('\\..*','', enemy_rows$species)

write.csv(friend_rows,'data/friend_rows.csv',row.names = FALSE)
write.csv(enemy_rows,'data/enemy_rows.csv',row.names = FALSE)

# strength of associations
#--------------------------------------------
# plot histograms per site
par(mfrow=c(2,2))
for (i in 1:4) {
 hist(as.matrix(a_obj[[i]][,2:ncol(a_obj[[i]])]))
} # looks like Gudmedalen (?) sticks out with a wider distribution of associations






