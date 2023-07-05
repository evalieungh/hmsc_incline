#################################################
#    gnmds axes analysis: climatic variables    #
#################################################

# Script by Eva L
# started 2023-01-16

{
library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams
library(plotly) # for 3D plotting
library(cowplot) # to combine plots
}

# GNMDS global (across-site) subplot scores vs...
# precipitation
# soil moisture
# soil temperature

# creates object used in plot_gnmds_covariate_vectors.R

# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/')
  # global ordination with k = 3 (three-dimensional solution)
mds <- readRDS('results/models/mds_k3.Rds') 
gnmds3_1 <- readRDS('results/models/ordination/gnmds3_1.Rds') # axis 1
gnmds3_2 <- readRDS('results/models/ordination/gnmds3_2.Rds')
gnmds3_3 <- readRDS('results/models/ordination/gnmds3_3.Rds')
mds1var <- readRDS('results/models/ordination/k3_mds1var.Rds') # species scores for axis 1
mds2var <- readRDS('results/models/ordination/k3_mds2var.Rds')
mds3var <- readRDS('results/models/ordination/k3_mds3var.Rds')
ord_df <- readRDS('data_processed/ord_df.Rds') # data frame with species occurrences

data_global <- data.frame(
  mds1 = gnmds3_1,
  mds2 = gnmds3_2,
  mds3 = gnmds3_3,
  ord_df,
  Site = ord_df$site,
  Precipitation <- factor(ord_df$prec)
)

# PRECIPITATION LEVEL
# -----------------------------
# plot axes and colour points by precipitation level (site)
( # plot global gnmds plot scores axis 1 and 2
  prec_plot_12 <- ggplot(data = data_global,
                         aes(
                           x = mds1,
                           y = mds2,
                           colour = Precipitation
                         )) +
    geom_point(
      size = 1,
      shape = 16,
      alpha = 1 / 2
    ) +
    xlab("GNMDS axis 1") +
    ylab("GNMDS axis 2") +
    coord_equal() +
    theme_classic()
)

(# plot global gnmds plot scores axis 1 and 3
  prec_plot_13 <- ggplot() +
    geom_point(
      data = data_global,
      aes(
        x = mds1,
        y = mds3,
        colour = Precipitation
      ),
      size = 1,
      shape = 16,
      alpha = 1/2
    ) +
    # guides(colour = "none") +
    xlab("GNMDS axis 1") +
    ylab("GNMDS axis 3") +
    coord_equal() +
    theme_classic()
)

# save both plots
(fig_S1 <- plot_grid(
  prec_plot_12,
  prec_plot_13,
  nrow = 2,
  labels = c('a', 'b'),
  align = "v"
))

ggsave(filename = "results/figures/figure_S1.png",
       plot = fig_S1,
       device = "png", 
       bg = "white",
       dpi = 300,
       width = 3,
       height = 4,
       scale = 1.5)

saveRDS(prec_plot_12,
        "data_processed/gnmds_global_prec_plot_12.Rds")
saveRDS(prec_plot_13,
        "data_processed/gnmds_global_prec_plot_13.Rds")

# alternative 3D plot, see https://plotly.com/r/reference/
plot_ly(x = ord_df$mds1,
        y = ord_df$mds2, 
        z = ord_df$mds3, 
        color = ord_df$prec,
        type="scatter3d", 
        mode="markers",
        size = 3) 
  # sites clearly separate, but not according to precipitation level.

# PRECIPITATION LEVELS
# -----------------------------------
# check correlations between ordination axis and precipitation level.
cor.test(gnmds3_1,data_global$prec,method="k") # tau=  0.324  p<2.2e-16
cor.test(gnmds3_2,data_global$prec,method="k") # tau= -0.334  p<2.2e-16
cor.test(gnmds3_3,data_global$prec,method="k") # tau= -0.181  p<2.2e-16

# calculate envfit vectors
prec_vector12 <- envfit(data_global[,c('mds1','mds2')], data_global$prec, permutations = 999)
prec_vector12 # prints coordinates for vector arrowheads and r-squared = 0.384
prec_vector13 <- envfit(data_global[,c('mds1','mds3')], data_global$prec, permutations = 999)
prec_vector13 # prints coordinates for vector arrowheads and r-squared = 0.303  

# save vectors as data frame
prec_vector12 <-
  as.data.frame(scores(prec_vector12, display = "vectors"))
prec_vector13 <-
  as.data.frame(scores(prec_vector13, display = "vectors"))

saveRDS(prec_vector12,
        "results/models/envfit_global_12_precipitation.Rds")
saveRDS(prec_vector13,
        "results/models/envfit_global_13_precipitation.Rds")

# SOIL MOISTURE
# ------------------------------
# other climatic variables might be more important than precipitation.
# check soil moisture and temperature
# See microclimate_formatting.R for data download and handling.

subplot_data <-
  readRDS("data_processed/subplot_data_sitespecific_list.Rds")

data_global2 <- rbind(subplot_data[[1]],
                      subplot_data[[2]],
                      subplot_data[[3]],
                      subplot_data[[4]])
data_global <-
  left_join(x = data_global,
            y = data_global2)

# check correlations between axes and soil moisture
cor.test(data_global$mds1, 
         data_global$soil_mst_mean, 
         method = "k") # tau=0.231   p<2.2e-16
cor.test(data_global$mds2, 
         data_global$soil_mst_mean, 
         method = "k") # tau=-0.071  p<2.2e-16
cor.test(data_global$mds3, 
         data_global$soil_mst_mean, 
         method = "k") # tau=-0.243  p<2.2e-16
{
  par(mfrow = c(1, 2))
  plot(data_global$mds1, data_global$soil_mst_mean)
  plot(data_global$mds3, data_global$soil_mst_mean)
}


# calculate envfit vectors
soilmst_subset <-
  subset(data_global,!is.na(data_global$soil_mst_mean))

(vector_soilmst_12 <-
    envfit(soilmst_subset[, c('mds1', 'mds2')], # ordination space
           soilmst_subset$soil_mst_mean, # environmental variable
           permutations = 999))
(vector_soilmst_13 <-
    envfit(soilmst_subset[, c('mds1', 'mds3')], # ordination space
           soilmst_subset$soil_mst_mean, # environmental variable
           permutations = 999))

vector_soilmst_12 <-
  as.data.frame(scores(vector_soilmst_12, display = "vectors"))
vector_soilmst_13 <-
  as.data.frame(scores(vector_soilmst_13, display = "vectors"))

saveRDS(vector_soilmst_12,
        "results/models/ordination/envfit_global_12_soilmst.Rds")
saveRDS(vector_soilmst_13,
        "results/models/ordination/envfit_global_13_soilmst.Rds")

# SOIL TEMPERATURE
# --------------------------------------------------
# check correlations between axes and soil temperature
cor.test(data_global$mds1, 
         data_global$soil_tmp_mean, 
         method = "k") # tau=0.544   p<2.2e-16
cor.test(data_global$mds2, 
         data_global$soil_tmp_mean, 
         method = "k") # tau=-0.17  p<2.2e-16
cor.test(data_global$mds3, 
         data_global$soil_tmp_mean, 
         method = "k") # tau=-0.022  p=0.236
{
  par(mfrow = c(1, 3))
  plot(data_global$mds1, data_global$soil_tmp_mean)
  plot(data_global$mds2, data_global$soil_tmp_mean)
  plot(data_global$mds3, data_global$soil_tmp_mean)
}

# calculate envfit vectors
soiltmp_subset <-
  subset(data_global,!is.na(data_global$soil_tmp_mean))

(vector_soiltmp_12 <-
    envfit(soiltmp_subset[, c('mds1', 'mds2')], # ordination space
           soiltmp_subset$soil_tmp_mean, # environmental variable
           permutations = 999))
(vector_soiltmp_13 <-
    envfit(soiltmp_subset[, c('mds1', 'mds3')], # ordination space
           soiltmp_subset$soil_tmp_mean, # environmental variable
           permutations = 999))

vector_soiltmp_12 <-
  as.data.frame(scores(vector_soiltmp_12, display = "vectors"))
vector_soiltmp_13 <-
  as.data.frame(scores(vector_soiltmp_13, display = "vectors"))

saveRDS(vector_soiltmp_12,
        "results/models/ordination/envfit_global_12_soiltmp.Rds")
saveRDS(vector_soiltmp_13,
        "results/models/ordination/envfit_global_13_soiltmp.Rds")

# --------------------------------------------------
# sources/inspiration: 
# Liu et al. 2008 Sommerfeltia
# https://chrischizinski.github.io/rstats/vegan-ggplot2/
# https://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2