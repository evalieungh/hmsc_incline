#########################################
#    INCLINE microclimate formatting    #
#########################################

# Script by Eva L
# started 2023-01-16

# data downloaded from INCLINE OSF
# https://osf.io/hrygk, INCLINE_microclimate.zip
# download date: 2023-01-16

# 1. inspect and clean data
# 1.1. soil moisture 
# 1.2. air temperature 
# 1.2. ground temperature 
# 1.3. soil temperature 
# 2. calculate growing season mean per plotID
# 3. format and merge into ordination data frame

{ # read libraries
library(tidyverse)# piping, formatting
library(dplyr) # group_by, summarise, joins
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
  '../../data/VCG/INCLINE_microclimate/data_cleaned/INCLINE_microclimate_ground_temperature.csv')
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
air_tmp$datetime <- ymd_hms(air_tmp$datetime) 
air_tmp$loggerID <- as.factor(air_tmp$loggerID) 
which(grepl("^\\s*$", air_tmp$plotID)==TRUE)
air_tmp <- air_tmp[air_tmp$OTC != 'W',] 
air_tmp <- air_tmp %>%
  add_column(date = date(air_tmp$datetime),
             hr = hour(air_tmp$datetime), 
             .before = 'loggerID')
  # 1.3. ground temperature
grnd_tmp$datetime <- ymd_hms(grnd_tmp$datetime) 
grnd_tmp$loggerID <- as.factor(grnd_tmp$loggerID) 
which(grepl("^\\s*$", grnd_tmp$plotID)==TRUE)
grnd_tmp <- grnd_tmp[grnd_tmp$OTC != 'W',] 
grnd_tmp <- grnd_tmp %>%
  add_column(date = date(grnd_tmp$datetime),
             hr = hour(grnd_tmp$datetime), 
             .before = 'loggerID')
  # 1.4. soil temperature
soil_tmp$datetime <- ymd_hms(soil_tmp$datetime) 
soil_tmp$loggerID <- as.factor(soil_tmp$loggerID) 
which(grepl("^\\s*$", soil_tmp$plotID)==TRUE)
soil_tmp <- soil_tmp[soil_tmp$OTC != 'W',] 
soil_tmp <- soil_tmp %>%
  add_column(date = date(soil_tmp$datetime),
             hr = hour(soil_tmp$datetime), 
             .before = 'loggerID')

# inspect data
  # 1.1. soil moisture
unique(year(soil_mst$datetime)) # 2019-2021
length(unique(soil_mst$loggerID)) # 63 loggers outside OTCs
soil_mst$datetime[1:50] # logging every 15 mins
  # 1.2. air temnperature = same as above
  # 1.3. ground temperature = same as above
  # 1.4. soil temperature = same as above

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
grnd_tmp2 <- grnd_tmp %>%
  group_by(plotID, date, hr) %>% # group by plot and interval
  summarise(grnd_tmp_h = mean(ground_temperature)) # get mean soil moisture per hour
grnd_tmp2 <- as.data.frame(grnd_tmp2)
  # 1.4. soil temperature
soil_tmp2 <- soil_tmp %>%
  group_by(plotID, date, hr) %>% # group by plot and interval
  summarise(soil_tmp_h = mean(soil_temperature)) # get mean soil moisture per hour
soil_tmp2 <- as.data.frame(soil_tmp2)

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
duplicated(air_tmp[1:50,])
air_tmp <- air_tmp %>% distinct()
  # 1.3. ground temperature
grnd_tmp <- left_join(grnd_tmp2,
                      grnd_tmp[,c('plotID','date', 'hr','siteID','loggerID','treatment')],
                      by = c('plotID', 'date', 'hr'))
duplicated(grnd_tmp[1:50,])
grnd_tmp <- grnd_tmp %>% distinct()
  # 1.4. soil temperature
soil_tmp <- left_join(soil_tmp2,
                      soil_tmp[,c('plotID','date', 'hr','siteID','loggerID','treatment')],
                      by = c('plotID', 'date', 'hr'))
duplicated(soil_tmp[1:50,])
soil_tmp <- soil_tmp %>% distinct()

# remove intermediate objects from R environment
remove(soil_mst2, air_tmp2, grnd_tmp2, soil_tmp2) 

# save modified data 
  # 1.1 soil moisture
saveRDS(soil_mst,'../../data_processed/soil_moisture_hourly_noOTC.Rds')
  # 1.2. air temnperature
saveRDS(air_tmp,'../../data_processed/air_temperature_hourly_noOTC.Rds')
  # 1.3. ground temperature
saveRDS(grnd_tmp,'../../data_processed/ground_temperature_hourly_noOTC.Rds')
  # 1.4. soil temperature
saveRDS(soil_tmp,'../../data_processed/soil_temperature_hourly_noOTC.Rds')

# Count how many plots ave loggers. There are 40*4=160 plots in total.
length(unique(soil_mst$plotID)) # 46 plots with data

# the community data is from 2018, but we only have microclimate data from 2019. 
# There are many species in this community that die back completely above ground 
# each year, but overwinter as roots. Which individuals grow back might vary 
# from year to year depending on that season's weather... But species survival 
# and community composition overall depends on more general climate and 
# variability over several years.
# Let's use the whole range to start with, to get closer to climate than weather.

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
plot_df <- grnd_tmp[grnd_tmp$date >= "2021-06-15", ]
plot_df <- as_tibble(plot_df[plot_df$siteID == 'Ulvehaugen',])
plot_df %>%
  ggplot(aes(x= date, y = grnd_tmp_h, 
             color = plotID)) +
  geom_point(size = 0.5, alpha = 0.7) +
  theme_classic()
  # 1.4. soil temperature
plot_df <- soil_tmp[soil_tmp$date >= "2021-06-15", ]
plot_df <- as_tibble(plot_df[plot_df$siteID == 'Ulvehaugen',])
plot_df %>%
  ggplot(aes(x= date, y = soil_tmp_h, 
             color = plotID)) +
  geom_point(size = 0.5, alpha = 0.7) +
  theme_classic()
  # it looks like ground temperature captures within-site variation best. 

# ridge plot per site
  # 1.1 soil moisture
soil_mst %>%
    ggplot(aes(x = date, y = siteID, fill = siteID)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")
  # 1.3. ground temperature
grnd_tmp %>%
  ggplot(aes(x = date, y = siteID, fill = siteID)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
  # 1.4. soil temperature
soil_tmp %>%
  ggplot(aes(x = date, y = siteID, fill = siteID)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

# 2. calculate growing season mean per plotID
# -----------------------------------------------
 # I have no use of the time series other than as independent 
  # background information. the most interesting data is the plot-specific 
  # average (or max and/or min?) for the growing season. 
  # This might consist of data from 0, 1 or more loggers!
  # To get this, I need to subset the growing seasons 
  # (assuming june--september), group by plotID,
  # calculate the mean (or max/min but let's start with mean),
  # and join the means to data frame to get one value per subPlotID

# define growing season date ranges
growing_season <-
  data.table(start = lubridate::date(c('2019-06-01',
                                       '2020-06-01',
                                       '2021-06-01')),
             end = lubridate::date(c('2019-09-30',
                                     '2020-09-30',
                                     '2021-09-30')))

# read the data
air_tmp  <- readRDS('../../data_processed/air_temperature_hourly_noOTC.Rds')
grnd_tmp <- readRDS('../../data_processed/ground_temperature_hourly_noOTC.Rds')
soil_tmp <- readRDS('../../data_processed/soil_temperature_hourly_noOTC.Rds')
soil_tmp[1:5,]
soil_mst <- readRDS('../../data_processed/soil_moisture_hourly_noOTC.Rds')
soil_mst[1:5,]

# subset only growing season
soil_mst_sub <- setDT(soil_mst)[date %inrange% growing_season]
soil_tmp_sub <- setDT(soil_tmp)[date %inrange% growing_season]
grnd_tmp_sub <- setDT(grnd_tmp)[date %inrange% growing_season]
air_tmp_sub  <- setDT(air_tmp)[date %inrange% growing_season]

# calculate means per plot ID
  # soil moisture
plot_mean_sm <- soil_mst_sub %>% # sm for soil moisture
  group_by(plotID) %>%
  summarise(soil_mst_mean = mean(na.omit(soil_moisture_h))) 
plot_mean_sm <- as.data.frame(plot_mean_sm)
  # air temp
plot_mean_at <- air_tmp_sub %>%
  group_by(plotID) %>%
  summarise(air_tmp_mean = mean(na.omit(air_tmp_h))) 
plot_mean_at <- as.data.frame(plot_mean_at)
  # ground temp
plot_mean_gt <- grnd_tmp_sub %>%
  group_by(plotID) %>%
  summarise(grnd_tmp_mean = mean(na.omit(grnd_tmp_h))) 
plot_mean_gt <- as.data.frame(plot_mean_gt)
  # soil temp
plot_mean_st <- soil_tmp_sub %>%
  group_by(plotID) %>%
  summarise(soil_tmp_mean = mean(na.omit(soil_tmp_h))) 
plot_mean_st <- as.data.frame(plot_mean_st)

# save means
write.csv(plot_mean_st, "../../data_processed/mean_soil_temp_per_plotID.csv", row.names = FALSE)
write.csv(plot_mean_gt, "../../data_processed/mean_ground_temp_per_plotID.csv", row.names = FALSE)
write.csv(plot_mean_at, "../../data_processed/mean_air_temp_per_plotID.csv", row.names = FALSE)
write.csv(plot_mean_sm, "../../data_processed/mean_soil_moisture_per_plotID.csv", row.names = FALSE)

# 3. format for ordination data frame
# ---------------------------------------

soil_mst <- read.csv("../../data_processed/mean_soil_moisture_per_plotID.csv")
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

# add soil moisture and temperature as new columns per site,
# by merging (joining) data by plotID
for (site in sites) {
  print(paste("adding soil temperature for: ", site))
  data_list[[site]] <- merge(
    x = data_list[[site]],
    all.x = TRUE,
    y = soil_t,
    by.x = 'plotID',
    by.y = 'plotID'
  )
  print(paste("adding soil moisture for: ", site))
  data_list[[site]] <- merge(
    x = data_list[[site]],
    all.x = TRUE,
    y = soil_mst,
    by.x = 'plotID',
    by.y = 'plotID'
  )
}

# reorder columns
data_list[["skj"]][1:5,1:10]
for (site in sites) {
  data_list[[site]] <- data_list[[site]] %>%
    select(site, blockID, plotID, subPlotID,
           prec,#mds1, mds2, 
           soil_tmp_mean, soil_mst_mean,
           everything())
}

saveRDS(data_list,
        '../../data_processed/subplot_data_sitespecific_list.Rds')
