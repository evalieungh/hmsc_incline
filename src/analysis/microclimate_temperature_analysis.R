##################################################
#  which temperature measurement explains most?  #
##################################################

# Script by Eva Lieungh
# started 2023-03-06

# We have microclimate measurements at air, ground, and soil. 
# Which temperature data should I proceed with? Environmental covariates should
# not be strongly correlated, but air/ground/soil temp obviously are. Which one
# is most ecologically relevant, captures the most variation, or shows small- 
# scale variation the best?

library(vegan) # envfit()

setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/')

# microclimate data from Tomst loggers. Source: INCLINE OSF
air_t <- read.csv('../../data_processed/mean_air_temp_per_plotID.csv')
ground_t <- read.csv('../../data_processed/mean_ground_temp_per_plotID.csv')
soil_t <- read.csv('../../data_processed/mean_soil_temp_per_plotID.csv')

# read in plot scores from site-specific ordination
mds_axes_wide <- read.csv("../../results/models/ordination/gnmds_axes_k2_sitespecific_wide.csv")

# read in site-specific ordination data frames in list
data_list <- list(
  skj = read.csv("../../data_processed/ord_df_skj.csv"),
  ulv = read.csv("../../data_processed/ord_df_ulv.csv"),
  lav = read.csv("../../data_processed/ord_df_lav.csv"),
  gud = read.csv("../../data_processed/ord_df_gud.csv")
)

# specify sites for looping
sites = c("skj", "ulv", "lav", "gud")

# add gnmds plot scores to site-specific data frames
for (site in sites) {
  print(site)
  data_list[[site]]$mds1 <- na.omit(mds_axes_wide[[paste0(site, "_1")]])
  data_list[[site]]$mds2 <- na.omit(mds_axes_wide[[paste0(site, "_2")]])
}

# add temperatures as new columns per site,
# by merging (joining) data by plotID
for (site in sites) {
  print(paste("adding air, ground, soil temperature for: ", site))
  data_list[[site]] <- merge(
    x = data_list[[site]],
    all.x = FALSE,
    y = air_t,
    by.x = 'plotID',
    by.y = 'plotID'
  )
  data_list[[site]] <- merge(
    x = data_list[[site]],
    all.x = FALSE,
    y = ground_t,
    by.x = 'plotID',
    by.y = 'plotID'
  )
  data_list[[site]] <- merge(
    x = data_list[[site]],
    all.x = FALSE,
    y = soil_t,
    by.x = 'plotID',
    by.y = 'plotID'
  )
}

# calculate envfit vectors and print r
vector_list = list()

for (site in sites) {
  print(paste("---", site, "---"))
  vector_list[[paste(site, "_air_temp")]] <-
    envfit(data_list[[site]][, c('mds1', 'mds2')],
           na.omit(data_list[[site]]$air_tmp_mean),
           permutations = 999)
  print(paste("Air temperature, envfit r: ",
              vector_list[[paste(site, "_air_temp")]]$vectors$r))
  
  vector_list[[paste(site, "_grnd_temp")]] <-
    envfit(data_list[[site]][, c('mds1', 'mds2')],
           na.omit(data_list[[site]]$grnd_tmp_mean),
           permutations = 999)
  print(paste("Ground temperature, envfit r: ",
              vector_list[[paste(site, "_grnd_temp")]]$vectors$r))
  
  vector_list[[paste(site, "_soil_temp")]] <-
    envfit(data_list[[site]][, c('mds1', 'mds2')],
           na.omit(data_list[[site]]$soil_tmp_mean),
           permutations = 999)
  print(paste("Soil temperature, envfit r: ",
              vector_list[[paste(site, "_soil_temp")]]$vectors$r))
}

# "--- skj ---"
# "Air temperature, envfit r:  0.0416541171995838"
# "Ground temperature, envfit r:  0.0176621802166269"
# "Soil temperature, envfit r:  0.0959477773491381"
# "--- ulv ---"
# "Air temperature, envfit r:  0.114717523445882"
# "Ground temperature, envfit r:  0.107354755277223"
# "Soil temperature, envfit r:  0.205758075432097"
# "--- lav ---"
# "Air temperature, envfit r:  0.376161134364954"
# "Ground temperature, envfit r:  0.302209896990168"
# "Soil temperature, envfit r:  0.309255331566739"
# "--- gud ---"
# "Air temperature, envfit r:  0.117530198940994"
# "Ground temperature, envfit r:  0.0742008100803676"
# "Soil temperature, envfit r:  0.144184061990155"

# Looks like soil temperature explains the most variation. 
# Visual inspection of time series of loggers also indicated 
# soil temperatures varied more within sites than the others
# (less overlap between loggers' ranges)