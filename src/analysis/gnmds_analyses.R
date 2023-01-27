#############################
#    gnmds axes analysis    #
#############################

# Script by Eva L
# started 2023-01-16

{library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams
library(plotly)} # for 3D plotting

# 1. precipitation
# 2. climatic predictors
# 3. sla, height, leaf area
# 4. co-occurrences

# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/')
mds <- readRDS('../../results/models/mds_k3.Rds') # ordination with k = 3 (three-dimensional solution)

gnmds3_1 <- readRDS('../../results/models/gnmds3_1.Rds') # axis 1
gnmds3_2 <- readRDS('../../results/models/gnmds3_2.Rds')
gnmds3_3 <- readRDS('../../results/models/gnmds3_3.Rds')

mds1var <- readRDS('../../results/models/k3_mds1var.Rds') # species scores for axis 1
mds2var <- readRDS('../../results/models/k3_mds2var.Rds')
mds3var <- readRDS('../../results/models/k3_mds3var.Rds')

ord_df <- readRDS('../../data_processed/ord_df.Rds') # data frame with ordination axes and species occurrences; to be modified further down
ord_df[1:5,1:10]

# 1. PRECIPITATION LEVELS
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

# 2. MICROCLIMATIC PREDICTORS
# ------------------------------
# other climatic variables might be more important than precipitation.
# See microclimate_formatting.R for data download and handling.
ord_df2 <- readRDS('../../data_processed/ord_df2.Rds')
ord_df2[1:5,1:10]

# check correlations between axes and ...
  # soil moisture
cor.test(ord_df2$mds1,ord_df2$soil_mst_mean,method="k") # tau=0.266   p<2.2e-16
cor.test(ord_df2$mds2,ord_df2$soil_mst_mean,method="k") # tau=-0.0270 p<2.2e-16
cor.test(ord_df2$mds3,ord_df2$soil_mst_mean,method="k") # tau=-0.227  p<2.2e-16
{par(mfrow=c(1,2))
plot(ord_df2$mds1,ord_df2$soil_mst_mean)
plot(ord_df2$mds3,ord_df2$soil_mst_mean)}
  # air temperature

  # ground temperature

  # soil temperature

# calculate envfit vectors
ord_df2_soilmst <- subset(ord_df2, !is.na(ord_df2$soil_mst_mean))
(v_sm_12 <- envfit(ord_df2_soilmst[,c('mds1','mds2')], # ordination space
                       ord_df2_soilmst$soil_mst_mean, # environmental variable
                       permutations = 999))
(v_sm_13 <- envfit(ord_df2_soilmst[,c('mds1','mds3')], # ordination space
                   ord_df2_soilmst$soil_mst_mean, # environmental variable
                   permutations = 999))
ord_df2_soiltmp <- # ... 

v_sm_12 <- as.data.frame(scores(v_sm_12, display = "vectors"))
# v_sm_13 <- as.data.frame(scores(v_sm_12, display = "vectors"))

plot_12 <- ggplot(ord_df2, 
                  aes(x = mds1, y = mds2, colour = Site)) + 
  coord_equal() +
  geom_point(size = 1, alpha = 0.3) +
  geom_segment(x = 0, y = 0,
               xend = v_sm_12$mds1,
               yend = v_sm_12$mds2,
               arrow = arrow(), colour = 'black') +
  theme_classic()

plot_13 <- ggplot(ord_df2, 
                  aes(x = mds1, y = mds3, colour = Site)) + 
  coord_equal() +
  geom_point(size = 1, alpha = 0.3) +
  geom_segment(x = 0, y = 0,
               xend = v_sm_13$mds1,
               yend = v_sm_13$mds2,
               arrow = arrow(), colour = 'black') +
  theme_classic()

# 3. SLA, HEIGHT, LEAF AREA
# ------------------------------
# compare measured traits to species scores

# load trait data (object saved from format_traits.R)
trait_df <- readRDS('data/trait_max.RData') # only 45 species

# make df of relative species scores
species.scores <- data.frame(mds1var,mds2var,mds3var)
species.scores$species <- rownames(species.scores)  # create a column of species

# join data, keeping only the 45 species with trait data
trait_mds <- left_join(trait_df,species.scores)

# calculate correlations between traits and axes
cor.test(trait_mds$mds1var,trait_mds$height,method="k") # tau=0.094  ties!
cor.test(trait_mds$mds1var,trait_mds$leaf_area,method="k") # tau=-0.0364 p=0.734
cor.test(trait_mds$mds1var,trait_mds$sla,method="k") # tau= -0.244 p=0.01778 *

cor.test(trait_mds$mds2var,trait_mds$height,method="k") # tau=0.153 ties!
cor.test(trait_mds$mds2var,trait_mds$leaf_area,method="k") # tau= 0.152 p=0.146
cor.test(trait_mds$mds2var,trait_mds$sla,method="k") # tau= 0.0687 p=0.514

cor.test(trait_mds$mds3var,trait_mds$height,method="k") # tau=0.288 ties!
cor.test(trait_mds$mds3var,trait_mds$leaf_area,method="k") # tau=-0.0485 p=0.647
cor.test(trait_mds$mds3var,trait_mds$sla,method="k") # tau=-0.204 p=0.0489

# plot species scores for 45 species (axes 1 vs 2 or 3)
(species_plot <- ggplot() + 
    geom_text(data=trait_mds,aes(x=mds1var,y=mds2var,label=species),alpha=0.5) +
    coord_equal() +
    theme_classic())
(species_plot2 <- ggplot() + 
    geom_text(data=trait_mds,aes(x=mds1var,y=mds3var,label=species),alpha=0.5) +
    coord_equal() +
    theme_classic())

# compute trait vectors
(traitvec12 <- envfit(trait_mds[,5:6], # mds1var and mds2var
                      trait_mds[,2:4], # trait columns
                      permutations = 999))
(traitvec13 <- envfit(trait_mds[,c(5,7)], # mds1var and mds3var
                      trait_mds[,2:4], # trait columns
                      permutations = 999))
traitvec12_df <- as.data.frame(scores(traitvec12, display = "vectors"))
traitvec12_df$trait <- rownames(traitvec12_df)
traitvec13_df <- as.data.frame(scores(traitvec13, display = "vectors"))
traitvec13_df$trait <- rownames(traitvec13_df)

# add trait vectors to plots
(species_plot <- ggplot() + 
    geom_text(data = trait_mds, # species scores
              aes(x = mds1var,y = mds2var,label = species), # axes 1,2
              size = 3, alpha = 0.7) + 
    geom_segment(data = traitvec12_df, # vector arrows
                 aes(x = 0, xend = mds1var, y = 0, yend = mds2var),
                 arrow = arrow(length = unit(0.5, "mm")), colour = "red") +
    geom_text(data = traitvec12_df, # vector labels
              aes(x = mds1var, y = mds2var, label = trait),
              size = 4) +
    coord_fixed() +
    labs(title = "GNMDS species scores and trait vectors",
         subtitle = "for 45 species with trait measurements", 
         x = "gnmds axis 1",
         y = "gnmds axis 2") +
    theme_classic())

(species_plot2 <- ggplot() + 
    geom_text(data = trait_mds, # species scores
              aes(x = mds1var,y = mds3var,label = species), # axes 1,3
              size = 3, alpha = 0.7) + 
    geom_segment(data = traitvec13_df, # vector arrows
                 aes(x = 0, xend = mds1var, y = 0, yend = mds3var),
                 arrow = arrow(length = unit(0.5, "mm")), 
                 colour = "red") +
    geom_text(data = traitvec13_df, # vector labels
              aes(x = mds1var, y = mds3var, label = trait),
              size = 4) +
    coord_fixed() +
    labs(title = "GNMDS species scores and trait vectors",
         subtitle = "for 45 species with trait measurements", 
         x = "gnmds axis 1",
         y = "gnmds axis 3") +
    theme_classic())

# 4. CO-OCCURRENCES
# ------------------------------
# get gnmds species score vs species score differences, test correlation to omegas
omegas_l <- readRDS('../../data_processed/omegas_long.Rds') # from format_omegas.R
mds1var <- readRDS('../../results/models/k3_mds1var.Rds') # species scores for axis 1
mds2var <- readRDS('../../results/models/k3_mds2var.Rds')
mds3var <- readRDS('../../results/models/k3_mds3var.Rds')

# compute differences in species scores per axis
mds1_diff <- sapply(mds1var, function(x) mds1var - x)
mds2_diff <- sapply(mds2var, function(x) mds2var - x)
mds3_diff <- sapply(mds3var, function(x) mds3var - x)

# set upper triangles to NA to avoid duplicates
mds1_diff[upper.tri(mds1_diff, diag=TRUE)] <- NA
mds2_diff[upper.tri(mds2_diff, diag=TRUE)] <- NA
mds3_diff[upper.tri(mds3_diff, diag=TRUE)] <- NA

# combine difference matrixes
mds_diffs <- data.frame(rbind(mds1_diff,
                              mds2_diff,
                              mds3_diff),
                        speciesA = rep(rownames(mds1_diff),3),
                        mds_axis = rep(1:3, each = nrow(mds1_diff)))
# write.csv(mds_diffs,'../../data_processed/mds_sp_scores_differences.csv')

# harmonise species lists by removing species not present in omega data
identical(unique(mds_diffs$speciesA),unique(omegas_l$speciesA))
specieslist_omega <- unique(omegas_l$speciesA)

mds_diffs <- mds_diffs[mds_diffs$speciesA %in% specieslist_omega,
                       c(which(colnames(mds_diffs) %in% specieslist_omega), 
                         ncol(mds_diffs) -1, ncol(mds_diffs))]

# pivot longer to get species pairs (A and B), and species score difference along each axis
mds_diffs <- mds_diffs %>%
  pivot_longer(Agr_cap:Vio_pal,
               names_to = 'speciesB',
               values_to = 'diff')

# remove rows with difference =NA
mds_diffs <- na.omit(mds_diffs)

# merge the two data sets, keeping all rows from both, with both species A and B as grouping vars
test <- full_join(x = omegas_l,
                  y = mds_diffs,
                  by = c("speciesA","speciesB"))

# check for obvious patterns
siteID = unique(test$siteID)
dev.new()
par(mfrow=c(4,3))
for (i in siteID) {
  plot(test[test$mds_axis==1 | test$siteID == i,]$diff,
       test[test$mds_axis==1 | test$siteID == i,]$omega)
  plot(test[test$mds_axis==2 | test$siteID == i,]$diff,
       test[test$mds_axis==2 | test$siteID == i,]$omega)
  plot(test[test$mds_axis==3 | test$siteID == i,]$diff,
       test[test$mds_axis==3 | test$siteID == i,]$omega)
}
dev.off()
# repeating patterns, no clear correlations. Why are the points identical? Are they identical?
# The mds is global, i.e. based on data across all sites. The omegas are site-specific.
for (i in siteID) {
  print(cor(na.omit(test[test$mds_axis==1 | test$siteID == i,]$diff),
            na.omit(test[test$mds_axis==1 | test$siteID == i,]$omega)))
  print(cor(na.omit(test[test$mds_axis==2 | test$siteID == i,]$diff),
            na.omit(test[test$mds_axis==2 | test$siteID == i,]$omega)))
  print(cor(na.omit(test[test$mds_axis==3 | test$siteID == i,]$diff),
            na.omit(test[test$mds_axis==3 | test$siteID == i,]$omega)))
}

# --------------------------------------------------
# sources/inspiration: 
# Liu et al. 2008 Sommerfeltia
# https://chrischizinski.github.io/rstats/vegan-ggplot2/
# https://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2