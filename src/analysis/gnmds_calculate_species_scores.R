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

ord_df

# GLOBAL
# --------------------------------------
# add ordination axes to df
ord_df <- ord_df %>%
  add_column(
    mds1 = gnmds3_1,
    mds2 = gnmds3_2,
    mds3 = gnmds3_3,
    .before = "Ach_mil"
  )
ord_df[1:5, 1:10]
saveRDS(ord_df, "../../data_processed/ord_df.Rds")
ord_df <- readRDS("../../data_processed/ord_df.Rds")

# for each species (cols 7:123) column (margin=2),
# sum the product of the species presences and
# the plot scores for an ordination axis, and divide this
# by the number of presences to get the species" average plot score per axis
mds1var <- apply(ord_df[, 7:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds1) / sum(x))
mds2var <- apply(ord_df[, 7:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds2) / sum(x))
mds3var <- apply(ord_df[, 7:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds3) / sum(x))

plot(dca1var, mds1var) # corresponds well to DCA results. Note scale difference

saveRDS(mds1var, "../../results/models/k3_mds1var.Rds")
saveRDS(mds2var, "../../results/models/k3_mds2var.Rds")
saveRDS(mds3var, "../../results/models/k3_mds3var.Rds")

# SITE-SPECIFIC
# collect ordination dataframes per site into list
ord_df_list = list(
  skj = ord_df_skj,
  ulv = ord_df_ulv,
  lav = ord_df_lav,
  gud = ord_df_gud
)

# add ordinatior axes to site-specific data frames
for (site in names(ord_df_list)) {
  tmp_ord_df = ord_df_list[[site]]
  print(site)
  mds1 = as.data.frame(na.omit(mds_axes_wide[, paste(site, "_1", sep = "")]))
  mds2 = as.data.frame(na.omit(mds_axes_wide[, paste(site, "_2", sep = "")]))
  ord_df_list[[site]]$mds1 <- flatten(mds1)
}

for (site in names(ord_df_list)) {
  
}
mds1var <- apply(ord_df[, 7:ncol(ord_df)], 2,
                 function(x)
                   sum(x * ord_df$mds1) / sum(x))


