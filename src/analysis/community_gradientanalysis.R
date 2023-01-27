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

# five ordinations with parallel methods:
# first all data ('global' analysis), 
# then per individual site.
# DCA and GNMDS. If the same axes appear in both methods, 
# they are more likely to represent real gradients in species composition.

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
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/')
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

# separate on site
ord_df_skj <- ord_df[ord_df$Site == 'Skjellingahaugen',]
ord_df_ulv <- ord_df[ord_df$Site == 'Ulvehaugen',]
ord_df_lav <- ord_df[ord_df$Site == 'Lavisdalen',]
ord_df_gud <- ord_df[ord_df$Site == 'Gudmedalen',]

# save dfs for later
write.csv(ord_df_skj,'../../data_processed/ord_df_skj.csv')
write.csv(ord_df_ulv,'../../data_processed/ord_df_ulv.csv')
write.csv(ord_df_lav,'../../data_processed/ord_df_lav.csv')
write.csv(ord_df_gud,'../../data_processed/ord_df_gud.csv')

# 2. DCA
# ------------------------------
# use decorana (de-trended cor-respondence ana-lysis) from J. Oksanen's vegan package
dca <- decorana(ord_df[,6:ncol(ord_df)]) # global analysis
dca_skj <- decorana(ord_df_skj[,6:ncol(ord_df_skj)]) # per site
dca_ulv <- decorana(ord_df_ulv[,6:ncol(ord_df_ulv)])
dca_lav <- decorana(ord_df_lav[,6:ncol(ord_df_lav)])
dca_gud <- decorana(ord_df_gud[,6:ncol(ord_df_gud)])

# preliminary plots of first 2 axes
plot(dca, main= 'global') 
{  dev.new()
  par(mfrow=c(2,2))
  plot(dca_skj, main = 'Skj')
  plot(dca_ulv, main = 'Ulv')
  plot(dca_lav, main = 'Lav')
  plot(dca_gud, main = 'Gud')
}
dev.off()

# extract DCA subplot scores
{  # global dca first
  dca1<-scores(dca,display="sites",origin=FALSE)[,1]# Note that origin=FALSE implies that origo of the ordination diagram is moved from the centroid to the lower end of each axis
  dca2<-scores(dca,display="sites",origin=FALSE)[,2]
  dca3<-scores(dca,display="sites",origin=FALSE)[,3]
  dca4<-scores(dca,display="sites",origin=FALSE)[,4]
}
{  # site-specific
dca1_skj<-scores(dca_skj,display="sites",origin=FALSE)[,1]
dca2_skj<-scores(dca_skj,display="sites",origin=FALSE)[,2]
dca3_skj<-scores(dca_skj,display="sites",origin=FALSE)[,3]
dca4_skj<-scores(dca_skj,display="sites",origin=FALSE)[,4]

dca1_ulv<-scores(dca_ulv,display="sites",origin=FALSE)[,1]
dca2_ulv<-scores(dca_ulv,display="sites",origin=FALSE)[,2]
dca3_ulv<-scores(dca_ulv,display="sites",origin=FALSE)[,3]
dca4_ulv<-scores(dca_ulv,display="sites",origin=FALSE)[,4]

dca1_lav<-scores(dca_lav,display="sites",origin=FALSE)[,1]
dca2_lav<-scores(dca_lav,display="sites",origin=FALSE)[,2]
dca3_lav<-scores(dca_lav,display="sites",origin=FALSE)[,3]
dca4_lav<-scores(dca_lav,display="sites",origin=FALSE)[,4]

dca1_gud<-scores(dca_gud,display="sites",origin=FALSE)[,1]
dca2_gud<-scores(dca_gud,display="sites",origin=FALSE)[,2]
dca3_gud<-scores(dca_gud,display="sites",origin=FALSE)[,3]
dca4_gud<-scores(dca_gud,display="sites",origin=FALSE)[,4]
}

# plot axes against each other
  # global
plot(dca1,dca2) # NB! This shows an artifact (~triangular shape)
plot(dca1,dca3)
  # site-specific
plot(dca1_skj,dca2_skj)
plot(dca1_skj,dca3_skj)

plot(dca1_ulv,dca2_ulv)
plot(dca1_ulv,dca3_ulv)

plot(dca1_lav,dca2_lav)
plot(dca1_lav,dca3_lav)

plot(dca1_gud,dca2_gud)
plot(dca1_gud,dca3_gud)

# calculate gradient lenghts
  # global
  (grl1<-max(dca1)-min(dca1)) # 5.3
  (grl2<-max(dca2)-min(dca2)) # 3.7
  (grl3<-max(dca3)-min(dca3)) # 3.6
  (grl4<-max(dca4)-min(dca4)) # 3.1
  # site-specific
grl_sites <- data.frame(site = rep(c('Skjellingahaugen', 'Ulvehaugen', 'Lavisdalen', 'Gudmedalen'),
                                   each = 4),
                        prec = rep(c(2725,593,1321,1925), each = 4),
                        axis = rep(1:4,4), 
                        length = c(grl1_skj = max(dca1_skj)-min(dca1_skj),
                                   grl2_skj = max(dca2_skj)-min(dca2_skj),
                                   grl3_skj = max(dca3_skj)-min(dca3_skj),
                                   grl4_skj = max(dca4_skj)-min(dca4_skj),

                                   grl1_ulv = max(dca1_ulv)-min(dca1_ulv),
                                   grl2_ulv = max(dca2_ulv)-min(dca2_ulv),
                                   grl3_ulv = max(dca3_ulv)-min(dca3_ulv),
                                   grl4_ulv = max(dca4_ulv)-min(dca4_ulv),

                                   grl1_lav = max(dca1_lav)-min(dca1_lav),
                                   grl2_lav = max(dca2_lav)-min(dca2_lav),
                                   grl3_lav = max(dca3_lav)-min(dca3_lav),
                                   grl4_lav = max(dca4_lav)-min(dca4_lav),

                                   grl1_gud = max(dca1_gud)-min(dca1_gud),
                                   grl2_gud = max(dca2_gud)-min(dca2_gud),
                                   grl3_gud = max(dca3_gud)-min(dca3_gud),
                                   grl4_gud = max(dca4_gud)-min(dca4_gud)))
write.csv(grl_sites,'../../data_processed/gradient_lengths_dca.csv')
plot(grl_sites$prec,grl_sites$length) # looks like the gradient lenght (indicating largest turnover in species composition) may have an optimum in medium-dry sites. 

# extract DCA species scores
{ # global
  dca1var<-scores(dca,display="species",origin=FALSE)[,1]
  dca2var<-scores(dca,display="species",origin=FALSE)[,2]
  dca3var<-scores(dca,display="species",origin=FALSE)[,3]
  dca4var<-scores(dca,display="species",origin=FALSE)[,4]
}
{ # site-specific
  dca1var_skj<-scores(dca_skj,display="species",origin=FALSE)[,1]
  dca2var_skj<-scores(dca_skj,display="species",origin=FALSE)[,2]
  dca3var_skj<-scores(dca_skj,display="species",origin=FALSE)[,3]
  dca4var_skj<-scores(dca_skj,display="species",origin=FALSE)[,4]
  
  dca1var_ulv<-scores(dca_ulv,display="species",origin=FALSE)[,1]
  dca2var_ulv<-scores(dca_ulv,display="species",origin=FALSE)[,2]
  dca3var_ulv<-scores(dca_ulv,display="species",origin=FALSE)[,3]
  dca4var_ulv<-scores(dca_ulv,display="species",origin=FALSE)[,4]
  
  dca1var_lav<-scores(dca_lav,display="species",origin=FALSE)[,1]
  dca2var_lav<-scores(dca_lav,display="species",origin=FALSE)[,2]
  dca3var_lav<-scores(dca_lav,display="species",origin=FALSE)[,3]
  dca4var_lav<-scores(dca_lav,display="species",origin=FALSE)[,4]
  
  dca1var_gud<-scores(dca_gud,display="species",origin=FALSE)[,1]
  dca2var_gud<-scores(dca_gud,display="species",origin=FALSE)[,2]
  dca3var_gud<-scores(dca_gud,display="species",origin=FALSE)[,3]
  dca4var_gud<-scores(dca_gud,display="species",origin=FALSE)[,4]
}

# 3. GNMDS
# ------------------------------
# Global nonmetric multidimensional scaling (GNMDS) ordination
# with geodetic correction of unreliable dissimilarities for PD > 0.8.
# First, make proportional dissimilarity (=Bray-Curtis) dissimilarity matrix
  # global
dist.y <- vegdist(ord_df[,4:ncol(ord_df)], method="bray")
saveRDS(dist.y,'../../data_processed/dist_y.Rds')
dist.y <- readRDS('../../data_processed/dist_y.Rds')
  # site-specific
dist.y.skj <- vegdist(ord_df_skj[,6:ncol(ord_df_skj)], method="bray")
dist.y.ulv <- vegdist(ord_df_ulv[,6:ncol(ord_df_ulv)], method="bray")
dist.y.lav <- vegdist(ord_df_lav[,6:ncol(ord_df_lav)], method="bray")
dist.y.gud <- vegdist(ord_df_gud[,6:ncol(ord_df_gud)], method="bray")

saveRDS(dist.y.skj,'../../data_processed/dist_y_skj.Rds')
saveRDS(dist.y.ulv,'../../data_processed/dist_y_ulv.Rds')
saveRDS(dist.y.lav,'../../data_processed/dist_y_lav.Rds')
saveRDS(dist.y.gud,'../../data_processed/dist_y_gud.Rds')

dist.y.skj <- readRDS('../../data_processed/dist_y_skj.Rds')
dist.y.ulv <- readRDS('../../data_processed/dist_y_ulv.Rds')
dist.y.lav <- readRDS('../../data_processed/dist_y_lav.Rds')
dist.y.gud <- readRDS('../../data_processed/dist_y_gud.Rds')

# Replace unreliable distances (B-C > 0.8 by geodetic distances, using the stepacross algorithm 
# Note that the optimal value for epsilon is dataset-specific. For data sets in which one or more observations are weakly related to the rest (disjunct data sets), geodetic correction does not work unless a lower value for epsilon is chosen. In such cases, find the highest value for epsilon that provides results
  # global
geodist.y <- isomapdist(dist.y, epsilon=0.8) # NB! this step takes a long time
saveRDS(geodist.y,'data/geodist_y.Rds')
geodist.y <- readRDS('../../data_processed/geodist_y.Rds')
  # site-specific
geodist.y.skj <- isomapdist(dist.y.skj, epsilon=0.8)
geodist.y.ulv <- isomapdist(dist.y.ulv, epsilon=0.8)
geodist.y.lav <- isomapdist(dist.y.lav, epsilon=0.8)
geodist.y.gud <- isomapdist(dist.y.gud, epsilon=0.8)

saveRDS(geodist.y.skj,'../../data_processed/geodist_y_skj.Rds')
saveRDS(geodist.y.ulv,'../../data_processed/geodist_y_ulv.Rds')
saveRDS(geodist.y.lav,'../../data_processed/geodist_y_lav.Rds')
saveRDS(geodist.y.gud,'../../data_processed/geodist_y_gud.Rds')

geodist.y.skj <- readRDS('../../data_processed/geodist_y_skj.Rds')
geodist.y.ulv <- readRDS('../../data_processed/geodist_y_ulv.Rds')
geodist.y.lav <- readRDS('../../data_processed/geodist_y_lav.Rds')
geodist.y.gud <- readRDS('../../data_processed/geodist_y_gud.Rds')

# select dimensionality for the GNMDS; k-dimensional GNMDS (k = 2, 3, ...).
# k determines the number of dimensions in the ordination; typically we start with the 4-dimensional solution, thereafter reduce the number of dimensions 
# (see axis correlations below - decided to re-run with 3 after initial test with 4)
k = 3

# define general, empty multi-dimensional-scaling object called mds:
mds <- NULL

mds_skj <- NULL
mds_ulv <- NULL
mds_lav <- NULL
mds_gud <- NULL

# make several MDSs (here 100) from random initial starting configurations,
# and allocate the solutions to the mds object.
# Remember to fit the right value of k into the statement 'k= ...' below
# NB! time-consuming. For the global data set, it takes >10 hrs, and the mds object is ~10 GB
  # global
for(i in 1:100) {
  mds[[i]] <- monoMDS(geodist.y,
                      matrix(c(runif(dim(ord_df[,6:ncol(ord_df)])[1]*k)),
                             nrow = dim(ord_df[,6:ncol(ord_df)])[1]),
                      k = 3, # number of dimensions (=axes) to use
                      model = "global", # "global" is normal non-metric MDS with a monotone regression
                      maxit = 200,
                      smin = 1e-7, sfgrmin = 1e-7)
} # The mds object is now a list consisting of 100 "sub-objects" which themselves are lists.
  # site-specific
for(i in 1:100) {
  mds_skj[[i]] <- monoMDS(geodist.y.skj,
                          matrix(c(runif(dim(ord_df_skj[,6:ncol(ord_df_skj)])[1]*k)),
                                 nrow = dim(ord_df_skj[,6:ncol(ord_df_skj)])[1]),
                          k = 3, model = "global", maxit = 200,
                          smin = 1e-7, sfgrmin = 1e-7)
}
for(i in 1:100) {
  mds_ulv[[i]] <- monoMDS(geodist.y.ulv,
                          matrix(c(runif(dim(ord_df_ulv[,6:ncol(ord_df_ulv)])[1]*k)),
                                 nrow = dim(ord_df_ulv[,6:ncol(ord_df_ulv)])[1]),
                          k = 3, model = "global", maxit = 200,
                          smin = 1e-7, sfgrmin = 1e-7)
}
for(i in 1:100) {
  mds_lav[[i]] <- monoMDS(geodist.y.lav,
                          matrix(c(runif(dim(ord_df_lav[,6:ncol(ord_df_lav)])[1]*k)),
                                 nrow = dim(ord_df_lav[,6:ncol(ord_df_lav)])[1]),
                          k = 3, model = "global", maxit = 200,
                          smin = 1e-7, sfgrmin = 1e-7)
}
for(i in 1:100) {
  mds_gud[[i]] <- monoMDS(geodist.y.gud,
                          matrix(c(runif(dim(ord_df_gud[,6:ncol(ord_df_gud)])[1]*k)),
                                 nrow = dim(ord_df_gud[,6:ncol(ord_df_gud)])[1]),
                          k = 3, model = "global", maxit = 200,
                          smin = 1e-7, sfgrmin = 1e-7)
}
# save/overwrite or load mds object 
saveRDS(mds,'../../results/models/mds_k3.Rds')
saveRDS(mds_skj,'../../results/models/mds_k3_skj.Rds')
saveRDS(mds_ulv,'../../results/models/mds_k3_ulv.Rds')
saveRDS(mds_lav,'../../results/models/mds_k3_lav.Rds')
saveRDS(mds_gud,'../../results/models/mds_k3_gud.Rds')

mds <- readRDS('../../results/models/mds_k3.Rds')
mds_skj <- readRDS('../../results/models/mds_k3_skj.Rds')
mds_ulv <- readRDS('../../results/models/mds_k3_ulv.Rds')
mds_lav <- readRDS('../../results/models/mds_k3_lav.Rds')
mds_gud <- readRDS('../../results/models/mds_k3_gud.Rds')

# Extract the stress values as a vector. 
# Stress values are provided by the 22th element in each "subobject list"
mds.stress <- unlist(lapply(mds,function(v){v[[22]]})) 

mds.stress.skj <- unlist(lapply(mds_skj,function(v){v[[22]]}))
mds.stress.ulv <- unlist(lapply(mds_ulv,function(v){v[[22]]}))
mds.stress.lav <- unlist(lapply(mds_lav,function(v){v[[22]]}))
mds.stress.gud <- unlist(lapply(mds_gud,function(v){v[[22]]}))

# view and order the stress values for the 100 MDSs:
mds.stress
ordered <- order(mds.stress) 
ordered

# get stress values for the solutions with the lowest and 2nd lowest stress
mds.stress[ordered[1]] # these two should be similar
mds.stress[ordered[2]] # large differences indicates artifact/local optima

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
