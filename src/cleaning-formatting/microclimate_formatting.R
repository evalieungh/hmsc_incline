#########################################
#    INCLINE microclimate formatting    #
#########################################

# Script by Eva L
# started 2023-01-16

# data downloaded from https://osf.io/hrygk, INCLINE_microclimate.zip
# download date: 2023-01-16

# 1. format to match ordination data frame
# 

library(tidyverse)# piping, formatting
library(lubridate) # date & time handling
library(timetk) # time series plotting
library(ggplot2) # nicer plotting
library(ggridges) # ridge plots


# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/')
air_temp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_air_temperature.csv')
grd_temp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleane/INCLINE_microclimate_ground_temperature.csv')
soil_mst <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_soil_moisture.csv')
soil_tmp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_soil_temperature.csv')

# start with soil moisture, which I suspect might be important

# format and filter out unused data
soil_mst$datetime <- ymd_hms(soil_mst$datetime) # convert to time format POSIXct, yyyy-mm-dd hh:mm:ss
soil_mst$loggerID <- as.factor(soil_mst$loggerID) # convert from integer to factor
which(grepl("^\\s*$", soil_mst$plotID)==TRUE) # search for whitespaces that might mess up further stuff
soil_mst <- soil_mst[soil_mst$OTC != 'W',] # remove warmed plots (OTC=open-top chamber)
soil_mst <- soil_mst %>% # Add date and hour as new columns
  add_column(date = date(soil_mst$datetime),
             hr = hour(soil_mst$datetime), 
             .before = 'loggerID')

# inspect data
unique(year(soil_mst$datetime)) # 2019-2021
length(unique(soil_mst$loggerID)) # 63 loggers outside OTCs
table(year(soil_mst$datetime),soil_mst$loggerID) # several loggers only have records the first 2 years, while others only have for the last year. Some have no values outside OTCs.
soil_mst$datetime[1:50] # logging every 15 mins

# get hourly average per plot
soil_mst2 <- soil_mst %>%
  group_by(plotID, date, hr) %>% # group by plot and interval
  summarise(soil_moisture_h = mean(soil_moisture)) # get mean soil moisture per hour
soil_mst2 <- as.data.frame(soil_mst2)

# now add back some columns from the original data frame. 
soil_mst <- left_join(soil_mst2,
                       soil_mst[,c('plotID','date', 'hr','siteID','loggerID','treatment')],
                       by = c('plotID', 'date', 'hr'))
duplicated(soil_mst[1:50,]) # not sure why but the join adds duplicated rows (i.e. keeps all the rows in y)
soil_mst <- soil_mst %>% distinct() # remove duplicates
remove(soil_mst2) # remove intermediate object from R environment

# save modified data 
saveRDS(soil_mst,'../../data_processed/soil_moisture_hourly_noOTC.Rds')

# the community data is from 2018, but we only have soil moisture data from 2019. There are many species in this community that die back completely above ground each year, but overwinter as roots. Which individuals grow back might vary from year to year depending on that season's weather... But species survival and community composition overall depends on more general climate and variability over several years.
# Let's use the whole range to start with.

# plot to get overview
soil_mst[soil_mst$siteID == 'Gudmedalen',] 
tribble %>%
  ggplot(aes(x= date, y = soil_moisture_h, 
             color = loggerID)) +
  geom_point(aes(alpha = 0.03)) 
  # clear differences between loggers indicating small-scale variation within sites

# test time series plotting
tribble <- soil_mst3[soil_mst3$date >= "2021-06-15", ]
tribble <- as_tibble(tribble[tribble$siteID == 'Gudmedalen',])
tribble %>% 
  plot_time_series(
    .date_var = date,
    .value    = soil_moisture_h,
    .smooth   = TRUE,
    .interactive = FALSE,
    .title = "Soil moisture")

# ridge plot per site
soil_mst3 %>%
    ggplot(aes(x = date, y = siteID, fill = siteID)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

# ideally, matching each logger to a plot would give the best approximation of conditions at the plot scale. But that would likely require a lot of subsetting and 

# 1. format to match ordination data frame
# ----------------------------------------------------
soil_mst <- readRDS('../../data_processed/soil_moisture_hourly_noOTC.Rds') # created above
soil_mst[1:10,]
ord_df <- readRDS('../../data_processed/ord_df.Rds') # made in community_gradientanalysis.R
ord_df[1:5,1:10]

# the ordination has no use of the time series other than as independent background information. 
# the most interesting data is the logger-specific average( or max and/or min?) for the growing season. 
# To get this, I need to subset the growing seasons (assuming june--september), 
# group by logger ID
# and calcualte the mean (or max/min but let's start with mean)
logger_means <- soil_mst %>%
  filter(date >= '' & date <= '')

#filter(DATE >= as.Date('2013-08-15') & DATE <= as.Date('2013-10-15'))

