###################################
#  CALCULATE NMDS SPECIES SCORES  #
###################################

# script by Eva Lieungh, Rune Halvorsen, Michal Torma
# started 2023-02-01

# In DCA, species scores (optimum along axes) are calculated first.
# Plot scores are then calculated as the mean of species scores for that plot.
# GNMDS instead finds plot scores first, and while we can find species scores
# these are relative and do not represent the estimated species optimum along
# the ordination axes. Here we calculate such relative species scores along
# each ordination axis.

library(tidyverse)

setwd(
  "C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/"
)

# GLOBAL
# --------------------------------------
# read input data frame for ordination
ord_df <- read.csv("data_processed/ordination_dataframe_global.csv")
gnmds3_1 <- readRDS("results/models/ordination/gnmds3_1.Rds")
gnmds3_2 <- readRDS("results/models/ordination/gnmds3_2.Rds")
gnmds3_3 <- readRDS("results/models/ordination/gnmds3_3.Rds")

# add ordination axes to df
ord_df <- ord_df %>%
  add_column(
    mds1 = gnmds3_1,
    mds2 = gnmds3_2,
    mds3 = gnmds3_3,
    .before = "Ach_mil"
  )
ord_df[1:5, 1:10]
saveRDS(ord_df, "/data_processed/ord_df.Rds")
ord_df <- readRDS("data_processed/ord_df.Rds")

# for each species (cols 7:123) column (margin=2),
# sum the product of the species presences and
# the plot scores for an ordination axis, and divide this
# by the number of presences to get the species" average plot score per axis
first_species_column = match("Ant_odo",names(ord_df))
mds1var <- apply(ord_df[, first_species_column:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds1) / sum(x))
mds2var <- apply(ord_df[, first_species_column:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds2) / sum(x))
mds3var <- apply(ord_df[, first_species_column:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds3) / sum(x))

saveRDS(mds1var, "results/models/ordination/k3_mds1var.Rds")
saveRDS(mds2var, "results/models/ordination/k3_mds2var.Rds")
saveRDS(mds3var, "results/models/ordination/k3_mds3var.Rds")

# check correspondence with DCA species scores
plot(dca1var, mds1var) # corresponds well to DCA results. Note scale difference

# SITE-SPECIFIC
# --------------------------------------
# read in necessary objects
mds_axes_wide <- read.csv("results/models/ordination/gnmds_axes_k2_sitespecific_wide.csv")

ord_df_skj <- read.csv("data_processed/ord_df_skj.csv")
ord_df_ulv <- read.csv("data_processed/ord_df_ulv.csv")
ord_df_lav <- read.csv("data_processed/ord_df_lav.csv")
ord_df_gud <- read.csv("data_processed/ord_df_gud.csv")

# collect ordination dataframes per site into list
ord_df_list = list(
  skj = ord_df_skj,
  ulv = ord_df_ulv,
  lav = ord_df_lav,
  gud = ord_df_gud
)

# add ordination axes to site-specific data frames
for (site in names(ord_df_list)) {
  print(site)
  mds1 = as.data.frame(na.omit(mds_axes_wide[, paste(site, "_1", sep = "")]))
  mds2 = as.data.frame(na.omit(mds_axes_wide[, paste(site, "_2", sep = "")]))
  ord_df_list[[site]]$mds1 <- flatten(mds1)
  ord_df_list[[site]]$mds2 <- flatten(mds2)
}

# reorder columns to get site, gnmds axes, etc first, then species columns
for (site in names(ord_df_list)) {
  ord_df_list[[site]] <- ord_df_list[[site]][, c(
    1:5,
    match("mds1", names(ord_df_list[[site]])),
    match("mds2", names(ord_df_list[[site]])),
    match("Ant_odo", names(ord_df_list[[site]])):ncol(ord_df_list[[site]])
  )]
  ord_df_list[[site]] <-
    ord_df_list[[site]][, c(1:(ncol(ord_df_list[[site]]) - 2))]
}

saveRDS(ord_df_list,"data_processed/ordination_df_sitespecific_list.Rds")

# for each site in the ordination df list, create a new variable 
# by calculating the species' average plot score per axis as
# the product of the species presences and the plot scores for an ordination axis,
# and divide this by the number of presences
species_scores = list()
for (site in names(ord_df_list)) {
  print(paste("calculating for site:", site))
  startcolumn = match("Ant_odo", names(ord_df_list[[site]]))
  print(paste("first species col:", startcolumn))
  stopcolumn = ncol(ord_df_list[[site]])
  print(paste("last species col:", stopcolumn))
  axis1 = unlist(ord_df_list[[site]]$mds1)
  axis2 = unlist(ord_df_list[[site]]$mds2)
  species_scores[[site]]$mds1 <-
    apply(ord_df_list[[site]][, startcolumn:stopcolumn],
          2,
          function(x)
            sum(x * axis1 / sum(x)))
  species_scores[[site]]$mds2 <-
    apply(ord_df_list[[site]][, startcolumn:stopcolumn],
          2,
          function(x)
            sum(x * axis2 / sum(x)))
}

saveRDS(species_scores,"results/gnmds_k2_species_scores_sitespecific.Rds")

# change NaN to NA
species_scores_list <- readRDS("results/gnmds_k2_species_scores_sitespecific.Rds")

sites = c("skj", "ulv", "lav", "gud")

for (site in sites) {
  species_scores_list[[site]]$mds1 <-
    sapply(species_scores_list[[site]]$mds1, function(x)
      replace(x, is.nan(x), NA))
  species_scores_list[[site]]$mds2 <-
    sapply(species_scores_list[[site]]$mds2, function(x)
      replace(x, is.nan(x), NA))
}

saveRDS(species_scores_list,"results/gnmds_k2_species_scores_sitespecific.Rds")

# further analyses and plotting in other scripts!
