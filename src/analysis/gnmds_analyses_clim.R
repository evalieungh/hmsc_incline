#################################################
#    gnmds axes analysis: climatic variables    #
#################################################

# Script by Eva L
# started 2023-01-16

{library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams
library(plotly)} # for 3D plotting

# 1. plot scores
# 2. precipitation
# 3. snow cover duration
# 4. soil moisture
# 5. air, ground, soil temperature

# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/')
  # global ordination with k = 3 (three-dimensional solution)
mds <- readRDS('../../results/models/mds_k3.Rds') 
gnmds3_1 <- readRDS('../../results/models/ordination/gnmds3_1.Rds') # axis 1
gnmds3_2 <- readRDS('../../results/models/ordination/gnmds3_2.Rds')
gnmds3_3 <- readRDS('../../results/models/ordination/gnmds3_3.Rds')
mds1var <- readRDS('../../results/models/ordination/k3_mds1var.Rds') # species scores for axis 1
mds2var <- readRDS('../../results/models/ordination/k3_mds2var.Rds')
mds3var <- readRDS('../../results/models/ordination/k3_mds3var.Rds')
ord_df <- readRDS('../../data_processed/ord_df.Rds') # data frame with ordination axes and species occurrences; to be modified further down

  # site-specific
mds_axes_long <- read.csv("../../results/models/ordination/gnmds_axes_k2_sitespecific.csv")
mds_axes_wide <- read.csv("../../results/models/ordination/gnmds_axes_k2_sitespecific_wide.csv")
ord_df_skj <- read.csv("../../data_processed/ord_df_skj.csv")
ord_df_ulv <- read.csv("../../data_processed/ord_df_ulv.csv")
ord_df_lav <- read.csv("../../data_processed/ord_df_lav.csv")
ord_df_gud <- read.csv("../../data_processed/ord_df_gud.csv")

# 1. PLOT ord_df_gudSCORES
# -----------------------------------


# 2. PRECIPITATION LEVELS
# -----------------------------------
# check correlations between ordination axis and precipitation level.
cor.test(gnmds3_1,ord_df$prec,method="k") # tau=0.355  p<2.2e-16
cor.test(gnmds3_3,ord_df$prec,method="k") # tau=-0.224 p<2.2e-16
cor.test(gnmds3_2,ord_df$prec,method="k") # tau=-0.297 p<2.2e-16

# calculate envfit vectors
prec_vector12 <- envfit(ord_df[,c('mds1','mds2')], ord_df$prec, permutations = 999)
prec_vector12 # prints coordinates for vector arrowheads and r-squared = 0.437
prec_vector13 <- envfit(ord_df[,c('mds1','mds3')], ord_df$prec, permutations = 999)
prec_vector13 # prints coordinates for vector arrowheads and r-squared = 0.373  

# plot axes and colour points by precipitation level (site)
ord_df$prec <- factor(ord_df$prec) # specify precipitation level as factor to use as grouping variable below
(prec_plot <- ggplot() + 
    geom_text(data=species.scores,aes(x=mds1var,y=mds2var,label=species),alpha=0.5) +
    geom_point(data=ord_df,
               aes(x=mds1,y=mds2,shape=prec,colour=prec),size=3) +
    coord_equal() +
    theme_classic())

(prec_plot <- ggplot() + 
    geom_point(data=ord_df,
               aes(x=mds1,y=mds3,shape=prec,colour=prec),size=3) +
    coord_equal() +
    theme_classic())

# 3D plot, see https://plotly.com/r/reference/
plot_ly(x = ord_df$mds1,
        y = ord_df$mds2, 
        z = ord_df$mds3, 
        color = ord_df$prec,
        type="scatter3d", 
        mode="markers",
        size = 3) 
  # sites clearly separate, but not according to precipitation level.

# SNOW COVER DURATION
# -----------------------------------
# from visual interpretation, axis 1 might correspond well to
# a gradient in snow cover duration (i.e. gradient from snowbed 
# to leeside vegetation)





# 2. SOIL MOISTURE
# ------------------------------
# other climatic variables might be more important than precipitation.
# check soil moisture and other microclimatic records
# See microclimate_formatting.R for data download and handling.
ord_df_clim <- readRDS('../../data_processed/ord_df_clim.Rds')
ord_df_clim[1:5,1:10]

# check correlations between axes and soil moisture
  # global
cor.test(ord_df_clim$mds1,ord_df_clim$soil_mst_mean,method="k") # tau=0.266   p<2.2e-16
cor.test(ord_df_clim$mds2,ord_df_clim$soil_mst_mean,method="k") # tau=-0.0270 p<2.2e-16
cor.test(ord_df_clim$mds3,ord_df_clim$soil_mst_mean,method="k") # tau=-0.227  p<2.2e-16
{par(mfrow=c(1,2))
plot(ord_df_clim$mds1,ord_df_clim$soil_mst_mean)
plot(ord_df_clim$mds3,ord_df_clim$soil_mst_mean)}

  # site-specific

# calculate envfit vectors
ord_df_clim_soilmst <- subset(ord_df_clim, !is.na(ord_df_clim$soil_mst_mean))
(v_sm_12 <- envfit(ord_df_clim_soilmst[,c('mds1','mds2')], # ordination space
                       ord_df_clim_soilmst$soil_mst_mean, # environmental variable
                       permutations = 999))
(v_sm_13 <- envfit(ord_df_clim_soilmst[,c('mds1','mds3')], # ordination space
                   ord_df_clim_soilmst$soil_mst_mean, # environmental variable
                   permutations = 999))
ord_df_clim_soiltmp <- # ... 

v_sm_12 <- as.data.frame(scores(v_sm_12, display = "vectors"))
# v_sm_13 <- as.data.frame(scores(v_sm_12, display = "vectors"))

plot_12 <- ggplot(ord_df_clim, 
                  aes(x = mds1, y = mds2, colour = Site)) + 
  coord_equal() +
  geom_point(size = 1, alpha = 0.3) +
  geom_segment(x = 0, y = 0,
               xend = v_sm_12$mds1,
               yend = v_sm_12$mds2,
               arrow = arrow(), colour = 'black') +
  theme_classic()

plot_13 <- ggplot(ord_df_clim, 
                  aes(x = mds1, y = mds3, colour = Site)) + 
  coord_equal() +
  geom_point(size = 1, alpha = 0.3) +
  geom_segment(x = 0, y = 0,
               xend = v_sm_13$mds1,
               yend = v_sm_13$mds2,
               arrow = arrow(), colour = 'black') +
  theme_classic()

# --------------------------------------------------
# sources/inspiration: 
# Liu et al. 2008 Sommerfeltia
# https://chrischizinski.github.io/rstats/vegan-ggplot2/
# https://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2