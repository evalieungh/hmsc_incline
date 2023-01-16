#########################################
#    INCLINE microclimate formatting    #
#########################################

# Script by Eva L
# started 2023-01-16

# data downloaded from https://osf.io/hrygk, INCLINE_microclimate.zip
# download date: 2023-01-16

# 1. format to match ordination data frame
# 

library(tidyverse)
library(lubridate)

# read data
air_temp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_air_temperature.csv')
grd_temp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleane/INCLINE_microclimate_ground_temperature.csv')
soil_mst <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_soil_moisture.csv')
soil_tmp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_soil_temperature.csv')

# start with soil moisture, which I suspect might be important
soil_mst$datetime <- ymd_hms(soil_mst$datetime) # convert to time format POSIXct, yyyy-mm-dd hh:mm:ss
soil_mst$loggerID <- as.factor(soil_mst$loggerID)

unique(year(soil_mst$datetime)) # 2019-2021
length(unique(soil_mst$loggerID)) # 108 loggers
table(year(soil_mst$datetime),soil_mst$loggerID) # several loggers only have records the first 2 years, while others only have for the last year. 

# view logging timesteps and get hourly average per plot
soil_mst$datetime[1:50] # logging every 15 mins
soil_mst2 <- soil_mst %>%
  mutate(hr = hour(datetime)) %>% 
  group_by(plotID, date= date(datetime), hr) %>% # group by plot and interval
  summarise(mean = mean(soil_moisture)) # get mean soil moisture

soil_mst2 <- as.data.frame(soil_mst2)

soil_mst3 <- left_join(soil_mst2,
                       soil_mst[,c('plotID','siteID','OTC','loggerID','treatment')]) # ERROR

# the community data is only from 2018, but we only have data from 2019. There are many species in this community that die back completely above ground each year, but overwinter as roots. Which individuals grow back might vary from year to year depending on that season's weather... But species survival and community composition overall depends on more general climate and variability over several years.
# Let's use the whole range to start with.

ggplot(soil_mst) +
  geom_point(aes(date, mean)) +
  facet_wrap(vars(plotID))

soil_mst %>%
  ggplot(aes(date, mean, group = , color = name)) +
  geom_line()

# ideally, matching each logger to a plot would give the best approximation of conditions at the plot scale. But that would likely require a lot of subsetting and 

# 1. format to match ordination data frame
# ----------------------------------------------------
ord_df <- readRDS('../../data_processed/ord_df.Rds') # made in community_gradientanalysis.R
ord_df[1:5,1:10]



