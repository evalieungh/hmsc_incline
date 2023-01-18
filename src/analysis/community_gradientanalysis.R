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

library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams

# 1. VIEW AND FORMAT DATA
# ------------------------------
# read clean and formatted community data
ord_df <- read.csv('../../data/VCG/INCLINE_community/INCLINE_community_2018_clean.csv')
ord_df[1:5,1:10]
# make precipitation column
ord_df <- ord_df %>%
  mutate(prec = as.integer(ifelse(Site=='Skjellingahaugen',2725,
                                  ifelse(Site=='Gudmedalen',1925,
                                         ifelse(Site=='Lavisdalen',1321,
                                                ifelse(Site=='Ulvehaugen',593,(.))))))) %>% 
  select(Site, prec, blockID, plotID, subPlotID, Ach_mil:Vio_sp)
ord_df[1:5,1:10]

# remove columns and rows without presences
row0 <- which(rowSums(ord_df[,6:ncol(ord_df)])==0);length(row0) # some subplots are empty
col0 <- which(colSums(ord_df[,6:ncol(ord_df)])==0) +5 ;length(col0) # some species are always absent (add 5 to get the correct column numbers in the col0 object, because we skip the first 5 columns)

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
dist.y <- readRDS('../../data_processed/dist_y.Rds')

# Replace unreliable distances (B-C > 0.8 by geodetic distances, using the stepacross algorithm 
# Note that the optimal value for epsilon is dataset-specific. For data sets in which one or more observations are weakly related to the rest (disjunct data sets), geodetic correction does not work unless a lower value for epsilon is chosen. In such cases, find the highest value for epsilon that provides results
# geodist.y <- isomapdist(dist.y, epsilon=0.8) # NB! this step takes a long time
# saveRDS(geodist.y,'data/geodist_y.Rds') 
geodist.y <- readRDS('../../data_processed/geodist_y.Rds')

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
# for(i in 1:100) {
#   mds[[i]] <- monoMDS(geodist.y, 
#                      matrix(c(runif(dim(ord_df[,4:ncol(ord_df)])[1]*k)),
#                             nrow = dim(ord_df[,4:ncol(ord_df)])[1]), 
#                      k = 3, model = "global", maxit = 200, 
#                      smin = 1e-7, sfgrmin = 1e-7)
# }
# The mds object is now a list consisting of 100 "sub-objects" which themselves are lists.

# save/overwrite or load mds object 
# saveRDS(mds,'../../results/models/mds_k3.Rds')
mds <- readRDS('../../results/models/mds_k3.Rds')

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

# save axes
saveRDS(gnmds3_1,'../../results/models/gnmds3_1.Rds')
saveRDS(gnmds3_2,'../../results/models/gnmds3_2.Rds')
saveRDS(gnmds3_3,'../../results/models/gnmds3_3.Rds')

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
saveRDS(ord_df,'../../data_processed/ord_df.Rds')

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

# save axes
saveRDS(mds1var,'../../results/models/k3_mds1var.Rds')
saveRDS(mds2var,'../../results/models/k3_mds2var.Rds')
saveRDS(mds3var,'../../results/models/k3_mds3var.Rds')

# further analysis in gnmds_analyses.R

# --------------------------------------------------
# sources/inspiration: 
# Liu et al. 2008 Sommerfeltia
