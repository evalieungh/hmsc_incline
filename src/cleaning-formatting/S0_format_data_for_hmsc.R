####################################
# Data formatting for HMSC scripts #
####################################

# script by Eva Lieungh
# started 2020

# Create an SXY file to comply with Hmsc scripts: 
# study design (S),
# covariates (X), and 
# species occurrences (Y)

# INCLINE_community_2018_clean.csv saved from community_data_cleaning.R
# subplot_data_sitespecific_list.Rds saved from microclimate_formatting.R

library(tidyverse)

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/cleaning-formatting/")

# Study design (S) and species occurrences (Y)
#------------------------------------------
# start SXY object from community data to be filled with more columns
# Each row is a subplot presence/absence from 2018.
# There are 29 subplots *40 plots *4 sites = 4640 rows
SY <- read.csv("../../data/VCG/INCLINE_community/INCLINE_community_2018_clean.csv")
SY[1:3,1:10]

# remove too rare or common species
Y = SY[,6:ncol(SY)] # extract species data
ubiquitous = round(nrow(Y) * 0.9) # define threshold for being too common
rare = round(nrow(Y) * 0.01) # define threshold for being too rare
print(paste("too rare if <", rare, "& too common if >", ubiquitous))
remove_pa = which(colSums(Y>0)<rare|colSums(Y>0)>ubiquitous)
print(paste("number of too rare or too common species: ",length(remove_pa)))
Y <- Y[,-remove_pa] # remove those species

colnames(Y)[grepl('_cf',colnames(Y))] # check for uncertain species
colnames(Y)[grepl('_sp',colnames(Y))] # check for genus-level records. Include for now

# final list of species to include
species <- c(names(Y))
write.csv(species,'Data/specieslist.csv',row.names = FALSE)

# save SY with the correct species
SY <- data.frame(SY[,1:5],Y) 

# Covariates (x)
#------------------------------------------
# make precipitation covariate, annual means (Gya et al. 2022)
SXY <- SY %>%
  mutate(prec = as.integer(ifelse(
    Site == 'Skjellingahaugen', 3402,
    ifelse(
      Site == 'Gudmedalen', 2130,
      ifelse(
        Site == 'Lavisdalen', 1561,
        ifelse(
          Site == 'Ulvehaugen', 1226, (.)))
    )
  )))

# soil moisture and temperature
microclimate_data_list <- 
  readRDS('../../data_processed/subplot_data_sitespecific_list.Rds')
microclimate_data_list[["skj"]][1:5,1:10]

# fill in NAs with site mean for soil moisture and temperature. Covariates cannot have NAs.
# NB! this will pull analyses in the direction of no response. 
# i.e. it might obscure signals from the data. 
# Any significant results/trends will likely be reliable, but harder to detect 
# (lower the analytical power of the method)
sites = c("skj", "ulv", "lav", "gud")

for (site in sites) {
  mean_soil_temp = mean(na.omit(microclimate_data_list[[site]]$soil_tmp_mean))
  NA_rows_temp = which(is.na(microclimate_data_list[[site]]$soil_tmp_mean))
  microclimate_data_list[[site]][NA_rows_temp,"soil_tmp_mean"] <-
    mean_soil_temp
  print(paste("mean soil temperature at", site, " is: ", mean_soil_temp))

  mean_soil_moisture = mean(na.omit(microclimate_data_list[[site]]$soil_mst_mean))
  NA_rows_mst = which(is.na(microclimate_data_list[[site]]$soil_mst_mean))
  microclimate_data_list[[site]][NA_rows_mst,"soil_mst_mean"] <-
    mean_soil_moisture
  print(paste("mean soil moisture at", site, " is: ", mean_soil_moisture))
  print(paste("number of NAs replaced:", length(NA_rows_mst)))
}

# mean soil temperature at skj  is:  10.4243350258643
# mean soil moisture at skj  is:  0.353139166009143
# number of NAs replaced: 812
# mean soil temperature at ulv  is:  9.0764489007919
# mean soil moisture at ulv  is:  0.263136979609348
# number of NAs replaced: 667
# mean soil temperature at lav  is:  8.15262006794624
# mean soil moisture at lav  is:  0.159625469438193
# number of NAs replaced: 725
# mean soil temperature at gud  is:  10.918808042157
# mean soil moisture at gud  is:  0.25086364215791
# number of NAs replaced: 841

# combine the relevant covariates for all sites
X <- do.call(rbind, lapply(microclimate_data_list, function(x)
  x[, c("subPlotID", "soil_tmp_mean", "soil_mst_mean")]))

# merge (left join) microclimate data back into SXY
SXY <- merge(
  x = SXY,
  all.x = TRUE,
  y = X,
  by.x = "subPlotID",
  by.y = "subPlotID"
)

SXY[86:120,c(1:5,64:68)] # missing for Gud_1_4 !
microclimate_data_list[["gud"]][microclimate_data_list[["gud"]]$plotID == "Gud_1_4",1:5]
# Missing already in ordination_dataframe_global.csv made in community_gradientanalysis.R

# other affected scripts:
# /hmsc_incline/src$ grep -rl "ord_df_gud.csv"
# analysis/community_gradientanalysis.R
# analysis/gnmds_analyses_clim.R
# analysis/gnmds_calculate_species_scores.R
# analysis/microclimate_tempersture_analysis.R
# cleaning-formatting/microclimate_formatting.R
  
# reorder columns
SXY[1:5, c(1:10,c(ncol(SXY)-5):ncol(SXY))]

SXY <- SXY %>%
select(Site:subPlotID,prec,Ach_mil:Vio_sp)
SXY[1:5,1:10]

# export SXY file
write.csv(SXY,"../../data/SXY.csv", row.names = FALSE)


# Reference
# Gya, R., Töpper, J.P., Olsen, S.L., Lieungh, E., Gaudard, J., Egelkraut, D.,
# ... & Vandvik, V. 
# "Indirect climate change impacts on alpine plant communities - 
# data from a warming, transplant and removal experiment". In: Gya, R. 2022. 
# "Disentangling effects and context dependencies of climate change on alpine plants"
# PhD thesis. University of Bergen, Norway.
