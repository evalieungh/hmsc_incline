#######################################
#    trait frequency distributions    #
#######################################

# script by Eva Lieungh
# started 2023-01-13

library(tidyverse) # piping, formatting
library(ggplot2) # nicer plotting
library(ggridges) # ridge plots

# read data (created in ../cleaning-formatting/format_traits.R)
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/')
tr_w <- readRDS('../data_processed/traits_wide_all.RData') # all traits in wide format

# make histograms
hist(tr_w$height_mm, main = 'Height, all sites')
{par(mfrow = c(2,2))
hist(subset(tr_w,tr_w$siteID=='Gudmedalen')$height_mm,
     xlab = 'height in mm', main = 'Gudmedalen', 
     xlim = c(0,500), ylim = c(0,110))
hist(subset(tr_w,tr_w$siteID=='Lavisdalen')$height_mm,
     xlab = 'height in mm', main = 'Lavisdalen', 
     xlim = c(0,500), ylim = c(0,110))
hist(subset(tr_w,tr_w$siteID=='Skjelingahaugen')$height_mm,
     xlab = 'height in mm', main = 'Skjelingahaugen', 
     xlim = c(0,500), ylim = c(0,110))
hist(subset(tr_w,tr_w$siteID=='Ulvehaugen')$height_mm,
     xlab = 'height in mm', main = 'Ulvehaugen', 
     xlim = c(0,500), ylim = c(0,110))}

hist(tr_w$SLA_cm2_g, main = 'SLA, all sites')
{par(mfrow = c(2,2))
  hist(subset(tr_w,tr_w$siteID=='Gudmedalen')$SLA_cm2_g,
       xlab = 'SLA, cm2_g', main = 'Gudmedalen', 
       xlim = c(0,550), ylim = c(0,100))
  hist(subset(tr_w,tr_w$siteID=='Lavisdalen')$SLA_cm2_g,
       xlab = 'SLA, cm2_g', main = 'Lavisdalen', 
       xlim = c(0,550), ylim = c(0,100))
  hist(subset(tr_w,tr_w$siteID=='Skjelingahaugen')$SLA_cm2_g,
       xlab = 'SLA, cm2_g', main = 'Skjelingahaugen', 
       xlim = c(0,550), ylim = c(0,100))
  hist(subset(tr_w,tr_w$siteID=='Ulvehaugen')$SLA_cm2_g,
       xlab = 'SLA, cm2_g', main = 'Ulvehaugen', 
       xlim = c(0,550), ylim = c(0,100))}

hist(tr_w$leaf_area_cm2, main = 'Leaf area, all sites')
{par(mfrow = c(2,2))
  hist(subset(tr_w,tr_w$siteID=='Gudmedalen')$leaf_area_cm2,
       xlab = 'leaf_area_cm2', main = 'Gudmedalen', 
       xlim = c(0,50), ylim = c(0,300))
  hist(subset(tr_w,tr_w$siteID=='Lavisdalen')$leaf_area_cm2,
       xlab = 'leaf_area_cm2', main = 'Lavisdalen', 
       xlim = c(0,50), ylim = c(0,300))
  hist(subset(tr_w,tr_w$siteID=='Skjelingahaugen')$leaf_area_cm2,
       xlab = 'leaf_area_cm2', main = 'Skjelingahaugen', 
       xlim = c(0,50), ylim = c(0,300))
  hist(subset(tr_w,tr_w$siteID=='Ulvehaugen')$leaf_area_cm2,
       xlab = 'leaf_area_cm2', main = 'Ulvehaugen', 
       xlim = c(0,50), ylim = c(0,300))}
  # redo for more traits if necessary

# mean trait values
siteIDs = c('Gudmedalen','Lavisdalen','Ulvehaugen','Skjelingahaugen')
for (i in siteIDs) {
  print(mean(na.omit(subset(tr_w,tr_w$siteID==i)$height_mm)))
}
for (i in siteIDs) {
  print(mean(na.omit(subset(tr_w,tr_w$siteID==i)$SLA_cm2_g)))
}
for (i in siteIDs) {
  print(mean(na.omit(subset(tr_w,tr_w$siteID==i)$leaf_area_cm2)))
}

# ridge plots (sets of density plots)
(heightplot <- tr_w %>%
  ggplot(aes(x = height_mm, y = siteID, fill = siteID)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none"))

(slaplot <- tr_w %>%
  ggplot(aes(x = SLA_cm2_g, y = siteID, fill = siteID)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none"))

(leafareaplot <- tr_w %>%
  ggplot(aes(x = leaf_area_cm2, y = siteID, fill = siteID)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none"))
# to do: add data points?
# https://r-graph-gallery.com/294-basic-ridgeline-plot.html

# frequency distribution of traits 
# should be scaled by abundances, so the most common/dominating species influence the distribution more


