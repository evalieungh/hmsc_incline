###############################################################
#  calculate occurrence sums per subplot, and envfit vectors  #
###############################################################

# script by Eva Lieungh
# started 2023-02-17

library(vegan) # envfit()

# read files
setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")

ord_df_list <- 
  readRDS("data_processed/ordination_df_sitespecific_list.Rds")

best_mds_list = list(
  skj = readRDS("results/models/mds_best_skj.Rds"),
  ulv = readRDS("results/models/mds_best_ulv.Rds"),
  lav = readRDS("results/models/mds_best_lav.Rds"),
  gud = readRDS("results/models/mds_best_gud.Rds")
)

sites = c("skj", "ulv", "lav", "gud")

# add columns with occurrence sums per subplot
  # site-specific
for (site in sites) {
  first_species_column = which(colnames(ord_df_list[[site]]) == "Ant_odo")
  last_species_column = ncol(ord_df_list[[site]])
  # calculate sum of presences per row
  ord_df_list[[site]]$occurrences <-
    apply(ord_df_list[[site]][, first_species_column:last_species_column],
          1,
          function(x)
            sum(x))
  # reorder columns to get new occurrence column before species
  ord_df_list[[site]] <-
    ord_df_list[[site]][, c(1:7,
                            which(colnames(ord_df_list[[site]]) == "occurrences"),
                            first_species_column:last_species_column)]
}

saveRDS(ord_df_list, "data_processed/ordination_df_sitespecific_list_occurrencesums.Rds")

# Calculate vectors for occurrence sums
  # site-specific
occurrence_vector_list = list()
for (site in sites) {
  occurrencesum_column_number = as.double(which(colnames(ord_df_list[[site]]) == "occurrences"))
  occurrence_vector_list[[site]] <-
    envfit(ord = best_mds_list[[site]],
           env = ord_df_list[[site]][, occurrencesum_column_number],
           permutations = 999)
}

saveRDS(occurrence_vector_list, "data_processed/occurrence_sums_sitespecific_vector_list.Rds")
