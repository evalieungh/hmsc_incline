#########################################
#    INCLINE microclimate formatting    #
#########################################

# Script by Eva L
# started 2023-01-16

# data downloaded from https://osf.io/hrygk, INCLINE_microclimate.zip
# download date: 2023-01-16

# 1. inspect and clean data
# 1.1. soil moisture 
# 1.2. air temperature 
# 1.2. ground temperature 
# 1.3. soil temperature 
# 2. format and merge into ordination data frame

{ # read libraries
library(tidyverse)# piping, formatting
library(lubridate) # date & time handling
library(timetk) # time series plotting
library(ggplot2) # nicer plotting
library(ggridges) # ridge plots
library(data.table) # to subset multiple date ranges effectively
}

# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/')
soil_mst <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_soil_moisture.csv')
air_tmp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_air_temperature.csv')
grnd_tmp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleane/INCLINE_microclimate_ground_temperature.csv')
soil_tmp <- read.csv(
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_soil_temperature.csv')

# 1. INSPECT AND CLEAN DATA
# ----------------------------------------------------------------
# format and filter out unused data
  # 1.1. soil moisture
soil_mst$datetime <- ymd_hms(soil_mst$datetime) # convert to time format POSIXct, yyyy-mm-dd hh:mm:ss
soil_mst$loggerID <- as.factor(soil_mst$loggerID) # convert from integer to factor
which(grepl("^\\s*$", soil_mst$plotID)==TRUE) # search for whitespaces that might mess up further stuff
soil_mst <- soil_mst[soil_mst$OTC != 'W',] # remove warmed plots (OTC=open-top chamber)
soil_mst <- soil_mst %>% # Add date and hour as new columns
  add_column(date = date(soil_mst$datetime),
             hr = hour(soil_mst$datetime), 
             .before = 'loggerID')
  # 1.2. air temnperature
air_tmp$datetime <- ymd_hms(air_tmp$datetime) # convert to time format POSIXct, yyyy-mm-dd hh:mm:ss
air_tmp$loggerID <- as.factor(air_tmp$loggerID) # convert from integer to factor
which(grepl("^\\s*$", air_tmp$plotID)==TRUE) # search for whitespaces that might mess up further stuff
air_tmp <- air_tmp[air_tmp$OTC != 'W',] # remove warmed plots (OTC=open-top chamber)
air_tmp <- air_tmp %>% # Add date and hour as new columns
  add_column(date = date(air_tmp$datetime),
             hr = hour(air_tmp$datetime), 
             .before = 'loggerID')
  # 1.3. ground temperature
  # 1.4. soil temperature

# inspect data
  # 1.1. soil moisture
unique(year(soil_mst$datetime)) # 2019-2021
length(unique(soil_mst$loggerID)) # 63 loggers outside OTCs
soil_mst$datetime[1:50] # logging every 15 mins
  # 1.2. air temnperature = same as above
  # 1.3. ground temperature
  # 1.4. soil temperature

# get hourly averages per plot
  # 1.1. soil moisture
soil_mst2 <- soil_mst %>%
  group_by(plotID, date, hr) %>% # group by plot and interval
  summarise(soil_moisture_h = mean(soil_moisture)) # get mean soil moisture per hour
soil_mst2 <- as.data.frame(soil_mst2)
  # 1.2. air temnperature
air_tmp2 <- air_tmp %>%
  group_by(plotID, date, hr) %>% # group by plot and interval
  summarise(air_tmp_h = mean(air_temperature)) # get mean soil moisture per hour
air_tmp2 <- as.data.frame(air_tmp2)
  # 1.3. ground temperature
  # 1.4. soil temperature

# now add back some columns from the original data frame. 
  # 1.1. soil moisture
soil_mst <- left_join(soil_mst2,
                       soil_mst[,c('plotID','date', 'hr','siteID','loggerID','treatment')],
                       by = c('plotID', 'date', 'hr'))
duplicated(soil_mst[1:50,]) # not sure why but the join adds duplicated rows (i.e. keeps all the rows in y)
soil_mst <- soil_mst %>% distinct() # remove duplicates
  # 1.2. air temnperature
air_tmp <- left_join(air_tmp2,
                     air_tmp[,c('plotID','date', 'hr','siteID','loggerID','treatment')],
                      by = c('plotID', 'date', 'hr'))
duplicated(air_tmp[1:50,]) # not sure why but the join adds duplicated rows (i.e. keeps all the rows in y)
air_tmp <- air_tmp %>% distinct() # remove duplicates
  # 1.3. ground temperature
  # 1.4. soil temperature

# remove intermediate objects from R environment
remove(soil_mst2, air_tmp2) 

# save modified data 
  # 1.1 soil moisture
saveRDS(soil_mst,'../../data_processed/soil_moisture_hourly_noOTC.Rds')
  # 1.2. air temnperature
saveRDS(air_tmp,'../../data_processed/air_temperature_hourly_noOTC.Rds')
  # 1.3. ground temperature
  # 1.4. soil temperature

# the community data is from 2018, but we only have soil moisture data from 2019. There are many species in this community that die back completely above ground each year, but overwinter as roots. Which individuals grow back might vary from year to year depending on that season's weather... But species survival and community composition overall depends on more general climate and variability over several years.
# Let's use the whole range to start with.

# plot to get overview
  # 1.1 soil moisture
plot_df <- soil_mst[soil_mst$date >= "2021-06-15", ]
plot_df <- as_tibble(plot_df[plot_df$siteID == 'Gudmedalen',])
plot_df %>%
  ggplot(aes(x= date, y = soil_moisture_h, 
             color = loggerID)) +
  geom_point(aes(alpha = 0.03)) 
    # clear differences between loggers indicating small-scale variation within sites
  # 1.2. air temperature
plot_df <- air_tmp[air_tmp$date >= "2021-06-15", ]
plot_df <- as_tibble(plot_df[plot_df$siteID == 'Ulvehaugen',])
plot_df %>%
  ggplot(aes(x= date, y = air_tmp_h, 
            color = plotID)) +
  geom_point(size = 0.5, alpha = 0.7) +
  theme_classic()
    # a lot of overlap between plot temperatures.
  # 1.3. ground temperature
  # 1.4. soil temperature

# ridge plot per site
  # 1.1 soil moisture
soil_mst %>%
    ggplot(aes(x = date, y = siteID, fill = siteID)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")
  # ... 

# 2. format to match ordination data frame
# ----------------------------------------------------
  # the ordination has no use of the time series other than as independent background information. 
  # the most interesting data is the plot-specific average( or max and/or min?) for the growing season. 
  # This might consist of data from 0, 1 or more loggers!
  # To get this, I need to subset the growing seasons (assuming june--september), 
  # group by plot ID
  # and calcualte the mean (or max/min but let's start with mean)

# define growing season date ranges
range <- data.table(start = lubridate::date(c('2019-06-01','2020-06-01','2021-06-01')), 
                    end = lubridate::date(c('2019-09-30','2020-09-30','2021-09-30')))

# read the data
ord_df <- readRDS('../../data_processed/ord_df.Rds') # made in community_gradientanalysis.R
ord_df[1:5,1:10]

soil_mst <- readRDS('../../data_processed/soil_moisture_hourly_noOTC.Rds') # created above
soil_mst[1:10,]

air_tmp <- readRDS('../../data_processed/air_temperature_hourly_noOTC.Rds')
air_tmp[1:10,]



# subset only growing season
soil_mst_sub <- setDT(soil_mst)[date %inrange% range]
air_tmp_sub <- setDT(air_tmp)[date %inrange% range]


# calculate means per plot ID
  # soil moisture
plot_means_sm <- soil_mst_sub %>% # sm for soil moisture
  group_by(plotID) %>%
  summarise(soil_mst_mean = mean(na.omit(soil_moisture_h))) 
plot_means_sm <- as.data.frame(plot_means_sm)
  # air temp
plot_means_at <- air_tmp_sub %>%
  group_by(plotID) %>%
  summarise(air_tmp_mean = mean(na.omit(air_tmp_h))) 
plot_means_at <- as.data.frame(plot_means_at)

# join new microclimate columns to ordination data frame
codes = c('plot_means_sm','plot_means_at')#,'plot_means_gt','plot_means_st')
for (i in 1:length(codes)) {
  ord_df2 <- merge(x = ord_df,
                 all.x = TRUE,
                 y = codes[i],
                 by.x = 'plotID',
                 by.y = 'plotID')
} # Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column


ord_df2 <- merge(x = ord_df,
                 all.x = TRUE,
                 y = plot_means_,
                 by = 'plotID')

# reorder columns
ord_df2 <- ord_df2 %>% 
  select(Site,prec,blockID,plotID,subPlotID:mds3,
         soil_mst_mean, everything())

saveRDS(ord_df2,'../../data_processed/ord_df2.Rds')


