##################################################
#    gradient analysis of species composition    #
##################################################

# script by Eva Lieungh, Rune Halvorsen

# data downloaded from INCLINE OSF repository 
# <link>
# prerequisite: community_cleaning script to get 
# subplot presences as rows, species as columns

# files needed to reproduce results:
# INCLINE_community_2018_clean.csv
# dist_y.Rds
# geodist_y.Rds
# mds_k3.Rds

# useful overview of ordination: https://ordination.okstate.edu/overview.htm

# 1. view and format data
# 2. DCA
# 3. GNMDS
# 4. Correlate DCA & GNMDS
# 5. Calculate NMDS species scores
# 6. Precipitation levels (incl. plot)
# 7. SLA and height (incl. plot)

library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams
library(plotly) # for 3D plotting

# 1. VIEW AND FORMAT DATA
# ------------------------------
# read clean and formatted community data
ord_df <- read.csv('data/Community/INCLINE_community_2018_clean.csv')
ord_df[1:5,1:10]
# make precipitation column
ord_df <- ord_df %>%
  mutate(prec = as.integer(ifelse(Site=='Skjellingahaugen',2725,
                                  ifelse(Site=='Gudmedalen',1925,
                                         ifelse(Site=='Lavisdalen',1321,
                                                ifelse(Site=='Ulvehaugen',593,(.))))))) %>% 
  select(Site, prec, subPlotID, Ach_mil:Vio_sp)
ord_df[1:5,1:10]

# remove columns and rows without presences
row0 <- which(rowSums(ord_df[,4:ncol(ord_df)])==0);length(row0) # some subplots are empty
col0 <- which(colSums(ord_df[,4:ncol(ord_df)])==0) +3 ;length(col0) # some species are always absent (add 3 to get the correct column numbers in the col0 object, because we skip the first 3 columns)
ord_df <- ord_df[-row0, -col0] # remove all-0 rows and columns
rm(row0,col0) # clean up environment

# 2. DCA
# ------------------------------
# use decorana (de-trended cor-respondence ana-lysis) from J. Oksanen's vegan package
dca <- decorana(ord_df[,4:ncol(ord_df)]) # 
plot(dca) # preliminary plot of first 2 axes

{# extract DCA subplot scores:
  dca1<-scores(dca,display="sites",origin=FALSE)[,1]# Note that origin=FALSE implies that origo of the ordination diagram is moved from the centroid to the lower end of each axis
  dca2<-scores(dca,display="sites",origin=FALSE)[,2]
  dca3<-scores(dca,display="sites",origin=FALSE)[,3]
  dca4<-scores(dca,display="sites",origin=FALSE)[,4]
}
# plot axes against each other
plot(dca1,dca2) # NB! This shows an artifact (~triangular shape)
plot(dca1,dca3)

{# calculate gradient lenghts
  (grl1<-max(dca1)-min(dca1)) # 5.3
  (grl2<-max(dca2)-min(dca2)) # 3.7
  (grl3<-max(dca3)-min(dca3)) # 3.6
  (grl4<-max(dca4)-min(dca4)) # 3.1
}

{# extract DCA species scores
  dca1var<-scores(dca,display="species",origin=FALSE)[,1]
  dca2var<-scores(dca,display="species",origin=FALSE)[,2]
  dca3var<-scores(dca,display="species",origin=FALSE)[,3]
  dca4var<-scores(dca,display="species",origin=FALSE)[,4]
}

# 3. GNMDS
# ------------------------------
# Global nonmetric multidimensional scaling (GNMDS) ordination
# with geodetic correction of unreliable dissimilarities for PD > 0.8.
# First, make proportional dissimilarity (=Bray-Curtis) dissimilarity matrix
# dist.y <- vegdist(ord_df[,4:ncol(ord_df)], method="bray") 
# saveRDS(dist.y,'data/dist_y.Rds')
dist.y <- readRDS('data/dist_y.Rds')
str(dist.y)

# Replace unreliable distances (B-C > 0.8 by geodetic distances, using the stepacross algorithm 
# Note that the optimal value for epsilon is dataset-specific. For data sets in which one or more observations are weakly related to the rest (disjunct data sets), geodetic correction does not work unless a lower value for epsilon is chosen. In such cases, find the highest value for epsilon that provides results
# geodist.y <- isomapdist(dist.y, epsilon=0.8) # NB! this step takes a long time
# saveRDS(geodist.y,'data/geodist_y.Rds') 
geodist.y <- readRDS('data/geodist_y.Rds')
str(geodist.y) 

# select dimensionality for the GNMDS; k-dimensional GNMDS (k = 2, 3, ...).
# k determines the number of dimensions in the ordination; typically we start with the 4-dimensional solution, thereafter reduce the number of dimensions 
# (see axis correlations below - decided to re-run with 2 after initial test with 4)
# trying again with k=2 and k=3
k = 3

# define a general, empty multi-dimensional-scaling object called mds:
mds <- NULL

# make several MDSs (here 100) from random initial starting configurations,
# and allocate the solutions to the mds object.
# Remember to fit the right value of k into the statement 'k= ...' below
# NB! time-consuming. For my data set, it takes >10 hrs, and the mds object is ~10 GB
for(i in 1:100) {
  mds[[i]] <- monoMDS(geodist.y, 
                     matrix(c(runif(dim(ord_df[,4:ncol(ord_df)])[1]*k)),
                            nrow = dim(ord_df[,4:ncol(ord_df)])[1]), 
                     k = 3, model = "global", maxit = 200, 
                     smin = 1e-7, sfgrmin = 1e-7)
}
# The mds object is now a list consisting of 100 "sub-objects" which themselves are lists.

# save/overwrite or load mds object 
saveRDS(mds,'data/mds_k3.Rds')
mds <- readRDS('data/mds_k3.Rds')

# Extract the stress values as a vector. 
# Stress values are provided by the 22th element in each "subobject list"
mds.stress <- unlist(lapply(mds,function(v){v[[22]]})) 

# view and order the stress values for the 100 MDSs:
mds.stress
ordered <- order(mds.stress) 
ordered

# get stress values for the solutions with the lowest and 2nd lowest stress:
mds.stress[ordered[1]]
mds.stress[ordered[2]]

# scale axes to half-change units and perform a varimax rotation by postMDS
mds.best <- postMDS(mds[[ordered[1]]],geodist.y, 
                    pc = TRUE, halfchange = TRUE, threshold = 0.8)
mds.best
mds.secbest <- postMDS(mds[[ordered[2]]],geodist.y, 
                       pc = TRUE, halfchange = TRUE, threshold = 0.8)
mds.secbest

# Procrustes comparisons
procrustes(mds.best,mds.secbest,permutations=999) # Procrustes sum of squares
  # 257.5 for k=2, 15.8 for k=3, 192.4 for K=4
protest(mds.best,mds.secbest,permutations=999)
  # Procrustes Sum of Squares (m12 squared):        0.008272
  # Correlation in a symmetric Procrustes rotation: 0.9959
  # Significance:  0.001
  # OK, the two best solutions are similar like they should be

# Procrustes plot
plot(procrustes(mds.best,mds.secbest,permutations=999))

# extract axes from lowest-stress mds
  # # k = 2
  #  gnmds2_1 <- mds.best$points[,1]
  #  gnmds2_2 <- mds.best$points[,2]
 # k = 3
 gnmds3_1 <- mds.best$points[,1]
 gnmds3_2 <- mds.best$points[,2]
 gnmds3_3 <- mds.best$points[,3]
  # #  k = 4
  # gnmds4_1 <- mds.best$points[,1]
  # gnmds4_2 <- mds.best$points[,2]
  # gnmds4_3 <- mds.best$points[,3]
  # gnmds4_4 <- mds.best$points[,4]

# plot axes against each other
plot(gnmds3_1,gnmds3_2)
plot(gnmds3_1,gnmds3_3)
  
# 4. CORRELATE DCA & GNMDS
# ------------------------------
# calculate Kendall's Tau non-parametric rank correlation coefficients 
  # Following Liu et al. (2008; <DOI>), 
  # we commonly regard a tau = 0.4 as a minimum for claiming that
  # two axes express more or less the same core of variation.
  # Preliminary mds run with k=4 gave cor.test(dca1,gnmds4_1,method="k") result 
  # tau = 0.8068197, p < 2.2e-16. Other axes had very low tau. 
  # Preliminary mds run with k=2 gave two 'strong' correlations: 
  # dca1,gnmds2_1 tau=-0.7821  p<2.2e-16
  # dca3,gnmds2_2 tau=0.3996  p<2.2e-16
  # but several others were close.
  # The final mds fitting with k=3 seems like the most appropriate:
cor.test(dca1,gnmds3_1,method="k") # tau=0.8108   p<2.2e-16 *
cor.test(dca1,gnmds3_2,method="k") # tau=0.0224   p=0.02846
cor.test(dca1,gnmds3_3,method="k") # tau=0.0551   p=6.763e-08

cor.test(dca2,gnmds3_1,method="k") # tau=-0.2179  p<2.2e-16
cor.test(dca2,gnmds3_2,method="k") # tau=-0.1435  p<2.2e-16
cor.test(dca2,gnmds3_3,method="k") # tau=0.5820   p<2.2e-16 *

cor.test(dca3,gnmds3_1,method="k") # tau=-0.1107  p<2.2e-16
cor.test(dca3,gnmds3_2,method="k") # tau=0.4458   p<2.2e-16 *
cor.test(dca3,gnmds3_3,method="k") # tau=0.1000   p<2.2e-16

cor.test(dca4,gnmds3_1,method="k") # tau=-0.0473  p=3.674e-06 
cor.test(dca4,gnmds3_2,method="k") # tau=0.2605   p<2.2e-16
cor.test(dca4,gnmds3_3,method="k") # tau=0.1582   p<2.2e-16

plot(dca1,gnmds3_1)
plot(dca2,gnmds3_3)
plot(dca3,gnmds3_2)
  # Both DCA and GNMDS pick up the same 3 axes, 
  # but DCA axis 2 corresponds to GNMDS axis 3 and vice versa. 
  # Because DCA2 shows an artifact, we choose the nmds axes for further analyses

# 5. CALCULATE NMDS SPECIES SCORES
# ------------------------------------
# In DCA, species scores (optimum along axes) are calculated first. 
# Plot scores are then calculated as the mean of species scores for that plot.
# GNMDS instead finds plot scores first, and while we can find species scores 
# these are relative and do not represent the estimated species optimum along 
# the ordination axes. Here we calculate such relative species scores along 
# each ordination axis.

# add ordination axes to df
ord_df <- ord_df%>%
  add_column(mds1 = gnmds3_1, 
             mds2 = gnmds3_2, 
             mds3 = gnmds3_3,
             .before = 'Ach_mil')
ord_df[1:5,1:10]

# for each species (cols 7:123) column (margin=2), 
# sum the product of the species presences and 
# the plot scores for an ordination axis, and divide this 
# by the number of presences to get the species' average plot score per axis
mds1var <- apply(ord_df[,7:ncol(ord_df)], 2,
                 function(x) sum(x * ord_df$mds1) / sum(x))
mds2var <- apply(ord_df[,7:ncol(ord_df)], 2,
                 function(x) sum(x * ord_df$mds2) / sum(x))
mds3var <- apply(ord_df[,7:ncol(ord_df)], 2,
                 function(x) sum(x * ord_df$mds3) / sum(x))

plot(dca1var,mds1var) # corresponds well to DCA results. Note scale difference

# 6. PRECIPITATION LEVELS
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
  
# 7. SLA AND HEIGHT
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

# --------------------------------------------------
# sources/inspiration: 
# Liu et al. 2008 Sommerfeltia
# https://chrischizinski.github.io/rstats/vegan-ggplot2/
# https://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2
