#############################
#    gnmds axes analysis    #
#############################

# Script by Eva L
# started 2023-01-16

library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams
library(plotly) # for 3D plotting

# 1. precipitation
# 2. climatic predictors
# 3. sla, height, leaf area
# 4. co-occurrences

# read data
mds <- readRDS('../../results/models/mds_k3.Rds') # ordination with k = 3 (three-dimensional solution)

gnmds3_1 <- readRDS('../../results/models/gnmds3_1.Rds') # axis 1
gnmds3_2 <- readRDS('../../results/models/gnmds3_2.Rds')
gnmds3_3 <- readRDS('../../results/models/gnmds3_3.Rds')

mds1var <- readRDS('../../results/models/k3_mds1var.Rds') # species scores for axis 1
mds2var <- readRDS('../../results/models/k3_mds2var.Rds')
mds3var <- readRDS('../../results/models/k3_mds3var.Rds')

ord_df <- readRDS('../../data_processed/ord_df.Rds') # data frame with ordination axes and species occurrences; to be modified further down

# 1. PRECIPITATION LEVELS
# -----------------------------------
# check correlations between ordination axis and precipitation level.
cor.test(gnmds3_1,ord_df$prec,method="k") # tau=0.355  p<2.2e-16
cor.test(gnmds3_3,ord_df$prec,method="k") # tau=-0.224 p<2.2e-16
cor.test(gnmds3_2,ord_df$prec,method="k") # tau=-0.297 p<2.2e-16

# calculate envfit vectors
prec_vector12 <- envfit(ord_df[,4:5], ord_df$prec, permutations = 999)
prec_vector12 # prints coordinates for vector arrowheads and r-squared = 0.437
prec_vector13 <- envfit(ord_df[,c(4,6)], ord_df$prec, permutations = 999)
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

# 2. SLA, HEIGHT, LEAF AREA
# ------------------------------
# other climatic variables might be more important than precipitation.
# Download local observations of available variables:
# 








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

# 2. CO-OCCURRENCES
# ------------------------------
# 
omegas <- 












# --------------------------------------------------
# sources/inspiration: 
# Liu et al. 2008 Sommerfeltia
# https://chrischizinski.github.io/rstats/vegan-ggplot2/
# https://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2