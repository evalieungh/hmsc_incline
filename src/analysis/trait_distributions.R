#######################################
#    trait frequency distributions    #
#######################################

# script by Eva Lieungh
# started 2023-01-13

library(ggplot2)

# read data (created in ../cleaning-formatting/format_traits.R)
tr_w <- readRDS('../../data_processed/traits_wide_all.RData') # all traits in wide format


# make histograms
hist(tr_w$height_mm)
par(mfrow = c(2,2))
hist(subset(tr_w,tr_w$siteID=='Gudmedalen')$height_mm,
     main = 'Gudmedalen', xlim = c(0,500))
hist(subset(tr_w,tr_w$siteID=='Lavisdalen')$height_mm,
     main = 'Lavisdalen', xlim = c(0,500))
hist(subset(tr_w,tr_w$siteID=='Skjelingahaugen')$height_mm,
     main = 'Skjelingahaugen', xlim = c(0,500))
hist(subset(tr_w,tr_w$siteID=='Ulvehaugen')$height_mm,
     main = 'Ulvehaugen', xlim = c(0,500))

# mean trait value
mean(na.omit(subset(tr_w,tr_w$siteID=='Gudmedalen')$height_mm))
mean(na.omit(subset(tr_w,tr_w$siteID=='Lavisdalen')$height_mm))
mean(na.omit(subset(tr_w,tr_w$siteID=='Skjelingahaugen')$height_mm))
mean(na.omit(subset(tr_w,tr_w$siteID=='Ulvehaugen')$height_mm))


# ridge plots (sets of density plots)
library(ggridges)
tr_w %>%
  ggplot(aes(x = height_mm, y = siteID, fill = siteID)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
# to do: add data points?
# https://r-graph-gallery.com/294-basic-ridgeline-plot.html

# frequency distribution of traits 
# should be scaled by abundances, so the most common/dominating species influence the distribution more


