##################################################
#    gradient analysis of species composition    #
##################################################

# script by Eva Lieungh, Rune Halvorsen
# started 2022-12 from Rune's script used in 
# BIOS9210 – Methods of Gradient Analysis

# R 4.2.3

# community data downloaded from INCLINE OSF repository
# prerequisite: community_cleaning script to get
# subplot presences as rows, species as columns

# five ordinations with parallel methods:
# first all data ("global" analysis),
# then for each of 4 sites.
# DCA and GNMDS. If the same axes appear in both methods,
# they are more likely to represent real gradients in species composition.

# 1. view and format data
# 2. DCA
# 3. GNMDS
# 4. Correlate DCA & GNMDS
# 5. Calculate NMDS species scores

library(tidyverse) # formatting, v 2.0.0
library(vegan) # gradient analysis, v 2.6-4

# 1. VIEW AND FORMAT DATA
# ------------------------------
# read clean and formatted community data
setwd(
  "C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/analysis/"
)
ord_df <-
  read.csv("../../data/VCG/INCLINE_community/INCLINE_community_2018_clean.csv")
ord_df[1:5, 1:10]

# make precipitation column (annual precipitation); see Gya 2022
ord_df <- ord_df %>%
  mutate(prec = as.integer(ifelse(
    site == "Skjellingahaugen", 3402,
    ifelse(
      site == "Gudmedalen",2130,
      ifelse(
        site == "Lavisdalen", 1561,
        ifelse(
          site == "Ulvehaugen", 1226, (.)))
    )
  ))) %>%
  select(site, prec, blockID, plotID, subPlotID, Ant_odo:Pru_vul)
ord_df[1:5, 1:10]

# save/load global ordination data frame
write.csv(ord_df,
          "../../data_processed/ordination_dataframe_global.csv",
          row.names = FALSE)
ord_df <- read.csv("../../data_processed/ordination_dataframe_global.csv")

# separate on site
ord_df_skj <- ord_df[ord_df$site == "Skjellingahaugen", ]
ord_df_ulv <- ord_df[ord_df$site == "Ulvehaugen", ]
ord_df_lav <- ord_df[ord_df$site == "Lavisdalen", ]
ord_df_gud <- ord_df[ord_df$site == "Gudmedalen", ]

# save dfs for later
write.csv(ord_df_skj,
          "../../data_processed/ord_df_skj.csv",
          row.names = FALSE)
write.csv(ord_df_ulv,
          "../../data_processed/ord_df_ulv.csv",
          row.names = FALSE)
write.csv(ord_df_lav,
          "../../data_processed/ord_df_lav.csv",
          row.names = FALSE)
write.csv(ord_df_gud,
          "../../data_processed/ord_df_gud.csv",
          row.names = FALSE)

# loading same objects
ord_df_skj <- read.csv("../../data_processed/ord_df_skj.csv")
ord_df_ulv <- read.csv("../../data_processed/ord_df_ulv.csv")
ord_df_lav <- read.csv("../../data_processed/ord_df_lav.csv")
ord_df_gud <- read.csv("../../data_processed/ord_df_gud.csv")

# 2. DCA
# ------------------------------
# use decorana (de-trended cor-respondence ana-lysis) from J. Oksanen"s vegan package
first_sp_col = which(colnames(ord_df) == "Ant_odo")
dca <- decorana(ord_df[, first_sp_col:ncol(ord_df)]) # global analysis
dca_skj <- decorana(ord_df_skj[, first_sp_col:ncol(ord_df_skj)]) # per site
dca_ulv <- decorana(ord_df_ulv[, first_sp_col:ncol(ord_df_ulv)])
dca_lav <- decorana(ord_df_lav[, first_sp_col:ncol(ord_df_lav)])
dca_gud <- decorana(ord_df_gud[, first_sp_col:ncol(ord_df_gud)]) # Warning message: In decorana(ord_df_gud[, 6:ncol(ord_df_gud)]) :  some species were removed because they were missing in the data

# save dca objects
saveRDS(dca, "../../results/models/dca_global.Rds")
saveRDS(dca_skj, "../../results/models/dca_skj.Rds")
saveRDS(dca_ulv, "../../results/models/dca_ulv.Rds")
saveRDS(dca_lav, "../../results/models/dca_lav.Rds")
saveRDS(dca_gud, "../../results/models/dca_gud.Rds")

# preliminary plots of first 2 axes
# global
plot(dca, main = "global")
# site-specific
{
  dev.new()
  par(mfrow = c(2, 2))
  plot(dca_skj, main = "Skj")
  plot(dca_ulv, main = "Ulv")
  plot(dca_lav, main = "Lav")
  plot(dca_gud, main = "Gud")
}
dev.off()

# extract DCA subplot scores
{
  # global dca first
  dca1 <-
    scores(dca, display = "sites", origin = FALSE)[, 1]# Note that origin=FALSE implies that origo of the ordination diagram is moved from the centroid to the lower end of each axis
  dca2 <- scores(dca, display = "sites", origin = FALSE)[, 2]
  dca3 <- scores(dca, display = "sites", origin = FALSE)[, 3]
  dca4 <- scores(dca, display = "sites", origin = FALSE)[, 4]
}

{
  # site-specific
  dca1_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 1]
  dca2_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 2]
  dca3_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 3]
  dca4_skj <- scores(dca_skj, display = "sites", origin = FALSE)[, 4]
  
  dca1_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 1]
  dca2_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 2]
  dca3_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 3]
  dca4_ulv <- scores(dca_ulv, display = "sites", origin = FALSE)[, 4]
  
  dca1_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 1]
  dca2_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 2]
  dca3_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 3]
  dca4_lav <- scores(dca_lav, display = "sites", origin = FALSE)[, 4]
  
  dca1_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 1]
  dca2_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 2]
  dca3_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 3]
  dca4_gud <- scores(dca_gud, display = "sites", origin = FALSE)[, 4]
}

# save plot scores
dca_obj_list <-
  list(
    dca1_skj = dca1_skj,
    dca2_skj = dca2_skj,
    dca3_skj = dca3_skj,
    dca4_skj = dca4_skj,
    dca1_ulv = dca1_ulv,
    dca2_ulv = dca2_ulv,
    dca3_ulv = dca3_ulv,
    dca4_ulv = dca4_ulv,
    dca1_lav = dca1_lav,
    dca2_lav = dca2_lav,
    dca3_lav = dca3_lav,
    dca4_lav = dca4_lav,
    dca1_gud = dca1_gud,
    dca2_gud = dca2_gud,
    dca3_gud = dca3_gud,
    dca4_gud = dca4_gud
  )
saveRDS(dca_obj_list,
        "../../data_processed/dca_list_plotscores_sitespecific.Rds")
dca_obj_list <-
  readRDS("../../data_processed/dca_list_plotscores_sitespecific.Rds")
dca_list = c(
  "dca1_skj",
  "dca2_skj",
  "dca3_skj",
  "dca4_skj",
  "dca1_ulv",
  "dca2_ulv",
  "dca3_ulv",
  "dca4_ulv",
  "dca1_lav",
  "dca2_lav",
  "dca3_lav",
  "dca4_lav",
  "dca1_gud",
  "dca2_gud",
  "dca3_gud",
  "dca4_gud"
)
for (i in dca_list) {
  write.csv(
    as.data.frame(dca_obj_list[i]),
    paste("../../data_processed/", i, ".csv", sep = ""),
    row.names = FALSE
  )
}

# read dca plot scores
dca_obj_list <-
  readRDS("../../data_processed/dca_list_plotscores_sitespecific.Rds") # global
for (i in 1:length(dca_list)) {
  # site-specific
  assign(paste(dca_list[i]),
         read.csv(paste(
           "../../data_processed/", dca_list[i], ".csv", sep = ""
         )))
}

# plot axes against each other
# global
plot(dca1, dca2) # NB! This shows an artifact (~triangular shape)
plot(dca1, dca3)
# site-specific
plot(dca1_skj, dca2_skj)
plot(dca1_skj, dca3_skj)

plot(dca1_ulv, dca2_ulv)
plot(dca1_ulv, dca3_ulv)

plot(dca1_lav, dca2_lav)
plot(dca1_lav, dca3_lav)

plot(dca1_gud, dca2_gud)
plot(dca1_gud, dca3_gud)

# check whether site-specific axes look similar to the global ones
# axes 1 and 2
{
  plot(
    x = dca1,
    y =  dca2,
    xlim = c(-0.2, 5),
    ylim = c(-0.2, 4),
    cex = 0.5,
    col = "grey",
    main = "site-specific plotted over global dca, axes 1 vs 2",
    sub = "black=Skj,blue=Ulv,green=Lav,red=Gud",
    xlab = "axis 1",
    ylab = "axis 2"
  )
  points(dca1_skj$dca1_skj,
         dca2_skj$dca2_skj,
         cex = 0.5,
         col = "black")
  points(dca1_ulv$dca1_ulv,
         dca2_ulv$dca2_ulv,
         cex = 0.5,
         col = "blue")
  points(dca1_lav$dca1_lav,
         dca2_lav$dca2_lav,
         cex = 0.5,
         col = "green")
  points(dca1_gud$dca1_gud,
         dca2_gud$dca2_gud,
         cex = 0.5,
         col = "red")
  abline(lm(dca1_skj$dca1_skj ~ dca2_skj$dca2_skj),
         lwd = 2,
         col = "black")
  abline(lm(dca1_ulv$dca1_ulv ~ dca2_ulv$dca2_ulv),
         lwd = 2,
         col = "blue")
  abline(lm(dca1_lav$dca1_lav ~ dca2_lav$dca2_lav),
         lwd = 2,
         col = "green")
  abline(lm(dca1_gud$dca1_gud ~ dca2_gud$dca2_gud),
         lwd = 2,
         col = "red")
}
# direct correlations of axes between the sites are not possible because they are
# different length (some species were omitted from analysis because they were
# missing from one site but not others). Could potentially go back and remove
# species that lack occurrences in some site(s) to force them to be comparable,
# but I deem it more important to keep the information in the species presences
# (i.e. the real species compositional differences between sites!).

# axes 1 and 3
{
  plot(
    x = dca1,
    y =  dca3,
    xlim = c(-0.2, 5),
    ylim = c(-0.2, 4),
    cex = 0.5,
    col = "grey",
    main = "site-specific plotted over global dca, axes 1 vs 3",
    sub = "black=Skj,blue=Ulv,green=Lav,red=Gud",
    xlab = "axis 1",
    ylab = "axis 3"
  )
  points(dca1_skj,
         dca3_skj,
         cex = 0.5,
         col = "black")
  points(dca1_ulv,
         dca3_ulv,
         cex = 0.5,
         col = "blue")
  points(dca1_lav,
         dca3_lav,
         cex = 0.5,
         col = "green")
  points(dca1_gud,
         dca3_gud,
         cex = 0.5,
         col = "red")
  abline(lm(dca1_skj ~ dca3_skj),
         lwd = 3,
         col = "black")
  abline(lm(dca1_ulv ~ dca3_ulv),
         lwd = 3,
         col = "blue")
  abline(lm(dca1_lav ~ dca3_lav),
         lwd = 3,
         col = "green")
  abline(lm(dca1_gud ~ dca3_gud),
         lwd = 3,
         col = "red")
}

# calculate gradient lenghts
# global
(grl1 <- max(dca1) - min(dca1)) # 5.1
(grl2 <- max(dca2) - min(dca2)) # 3.8
(grl3 <- max(dca3) - min(dca3)) # 3.9
(grl4 <- max(dca4) - min(dca4)) # 2.1
# site-specific
grl_sites <-
  data.frame(
    site = rep(
      c("Skjellingahaugen", "Ulvehaugen", "Lavisdalen", "Gudmedalen"),
      each = 4
    ),
    prec = rep(c(3402, 1226, 1561, 2130), each = 4),
    axis = rep(1:4, 4),
    length = c(
      grl1_skj = max(dca1_skj) - min(dca1_skj),
      grl2_skj = max(dca2_skj) - min(dca2_skj),
      grl3_skj = max(dca3_skj) - min(dca3_skj),
      grl4_skj = max(dca4_skj) - min(dca4_skj),
      
      grl1_ulv = max(dca1_ulv) - min(dca1_ulv),
      grl2_ulv = max(dca2_ulv) - min(dca2_ulv),
      grl3_ulv = max(dca3_ulv) - min(dca3_ulv),
      grl4_ulv = max(dca4_ulv) - min(dca4_ulv),
      
      grl1_lav = max(dca1_lav) - min(dca1_lav),
      grl2_lav = max(dca2_lav) - min(dca2_lav),
      grl3_lav = max(dca3_lav) - min(dca3_lav),
      grl4_lav = max(dca4_lav) - min(dca4_lav),
      
      grl1_gud = max(dca1_gud) - min(dca1_gud),
      grl2_gud = max(dca2_gud) - min(dca2_gud),
      grl3_gud = max(dca3_gud) - min(dca3_gud),
      grl4_gud = max(dca4_gud) - min(dca4_gud)
    )
  )
write.csv(grl_sites, "../../data_processed/gradient_lengths_dca.csv")
plot(grl_sites$prec, grl_sites$length) # longer gradients in dryer sites?

# extract DCA species scores
{
  # global
  dca1var <- scores(dca, display = "species", origin = FALSE)[, 1]
  dca2var <- scores(dca, display = "species", origin = FALSE)[, 2]
  dca3var <- scores(dca, display = "species", origin = FALSE)[, 3]
  dca4var <- scores(dca, display = "species", origin = FALSE)[, 4]
}
{
  # site-specific
  dca1var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 1]
  dca2var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 2]
  dca3var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 3]
  dca4var_skj <- scores(dca_skj, display = "species", origin = FALSE)[, 4]
  
  dca1var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 1]
  dca2var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 2]
  dca3var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 3]
  dca4var_ulv <- scores(dca_ulv, display = "species", origin = FALSE)[, 4]
  
  dca1var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 1]
  dca2var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 2]
  dca3var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 3]
  dca4var_lav <- scores(dca_lav, display = "species", origin = FALSE)[, 4]
  
  dca1var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 1]
  dca2var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 2]
  dca3var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 3]
  dca4var_gud <- scores(dca_gud, display = "species", origin = FALSE)[, 4]
}

# save species scores
dca_species_scores_sitespecific <-
  data.frame(
    dca1var_skj,
    dca2var_skj,
    dca3var_skj,
    dca4var_skj,
    dca1var_ulv,
    dca2var_ulv,
    dca3var_ulv,
    dca4var_ulv,
    dca1var_lav,
    dca2var_lav,
    dca3var_lav,
    dca4var_lav,
    dca1var_gud,
    dca2var_gud,
    dca3var_gud,
    dca4var_gud
  )

write.csv(dca_species_scores_sitespecific,
          "../../results/dca_species_scores_sitespecific.csv",
          row.names = TRUE)

# 3. GNMDS
# ------------------------------
# Global nonmetric multidimensional scaling (GNMDS) ordination
# with geodetic correction of unreliable dissimilarities for PD > 0.8.
ord_df <- read.csv("../../data_processed/ordination_dataframe_global.csv")
ord_df_skj <- read.csv("../../data_processed/ord_df_skj.csv")
ord_df_ulv <- read.csv("../../data_processed/ord_df_ulv.csv")
ord_df_lav <- read.csv("../../data_processed/ord_df_lav.csv")
ord_df_gud <- read.csv("../../data_processed/ord_df_gud.csv")

# First, make proportional dissimilarity (=Bray-Curtis) dissimilarity matrix
# global
dist.y <- vegdist(ord_df[, 6:ncol(ord_df)], method = "bray")
saveRDS(dist.y, "../../data_processed/dist_y.Rds")
dist.y <- readRDS("../../data_processed/dist_y.Rds")
# site-specific
dist.y.skj <-
  vegdist(ord_df_skj[, 6:ncol(ord_df_skj)], method = "bray")
dist.y.ulv <-
  vegdist(ord_df_ulv[, 6:ncol(ord_df_ulv)], method = "bray")
dist.y.lav <-
  vegdist(ord_df_lav[, 6:ncol(ord_df_lav)], method = "bray")
dist.y.gud <-
  vegdist(ord_df_gud[, 6:ncol(ord_df_gud)], method = "bray")

saveRDS(dist.y.skj, "../../data_processed/dist_y_skj.Rds")
saveRDS(dist.y.ulv, "../../data_processed/dist_y_ulv.Rds")
saveRDS(dist.y.lav, "../../data_processed/dist_y_lav.Rds")
saveRDS(dist.y.gud, "../../data_processed/dist_y_gud.Rds")

dist.y.skj <- readRDS("../../data_processed/dist_y_skj.Rds")
dist.y.ulv <- readRDS("../../data_processed/dist_y_ulv.Rds")
dist.y.lav <- readRDS("../../data_processed/dist_y_lav.Rds")
dist.y.gud <- readRDS("../../data_processed/dist_y_gud.Rds")

# Replace unreliable distances (B-C > 0.8 by geodetic distances, 
# using the stepacross algorithm.
# Note that the optimal value for epsilon is dataset-specific. 
# For data sets in which one or more observations are weakly 
# related to the rest (disjunct data sets), geodetic correction 
# does not work unless a lower value for epsilon is chosen. 
# In such cases, find the highest value for epsilon that provides results
# global
geodist.y <-
  isomapdist(dist.y, epsilon = 0.8) # NB! this step takes a long time (15-20mins)
saveRDS(geodist.y, "../../data_processed/geodist_y.Rds")
geodist.y <- readRDS("../../data_processed/geodist_y.Rds")
# site-specific
geodist.y.skj <- isomapdist(dist.y.skj, epsilon = 0.8)
geodist.y.ulv <- isomapdist(dist.y.ulv, epsilon = 0.8)
geodist.y.lav <- isomapdist(dist.y.lav, epsilon = 0.8)
geodist.y.gud <- isomapdist(dist.y.gud, epsilon = 0.8)

saveRDS(geodist.y.skj, "../../data_processed/geodist_y_skj.Rds")
saveRDS(geodist.y.ulv, "../../data_processed/geodist_y_ulv.Rds")
saveRDS(geodist.y.lav, "../../data_processed/geodist_y_lav.Rds")
saveRDS(geodist.y.gud, "../../data_processed/geodist_y_gud.Rds")

geodist.y.skj <- readRDS("../../data_processed/geodist_y_skj.Rds")
geodist.y.ulv <- readRDS("../../data_processed/geodist_y_ulv.Rds")
geodist.y.lav <- readRDS("../../data_processed/geodist_y_lav.Rds")
geodist.y.gud <- readRDS("../../data_processed/geodist_y_gud.Rds")

# select dimensionality for the GNMDS; k-dimensional GNMDS (k = 2, 3, ...).
# k determines the number of dimensions in the ordination; typically we start with the 4-dimensional solution, thereafter reduce the number of dimensions
# (see axis correlations below - decided to re-run global with 3 after initial test with 4, and site-specific with 2 after initial test with 3)
k.glob = 3
k.site = 2

# define general, empty multi-dimensional-scaling object called mds:
mds <- NULL

mds_skj <- NULL
mds_ulv <- NULL
mds_lav <- NULL
mds_gud <- NULL

# make several MDSs (here 100) from random initial starting configurations,
# and allocate the solutions to the mds object.
# Remember to fit the right value of k into the statement "k= ..." below
# NB! time-consuming. For the global data set, it takes >10 hrs, and the mds object is ~10 GB
# global
for (i in 1:100) {
  mds[[i]] <- monoMDS(
    geodist.y,
    matrix(c(runif(
      dim(ord_df[, 6:ncol(ord_df)])[1] * k.glob
    )),
    nrow = dim(ord_df[, 6:ncol(ord_df)])[1]),
    k = k.glob,
    # number of dimensions (=axes) to use
    model = "global",
    # "global" is normal non-metric MDS with a monotone regression
    maxit = 200,
    smin = 1e-7,
    sfgrmin = 1e-7
  )
} # The mds object is now a list consisting of 100 "sub-objects" which themselves are lists.
#12:40--
# site-specific
for (i in 1:100) {
  mds_skj[[i]] <- monoMDS(
    geodist.y.skj,
    matrix(c(runif(
      dim(ord_df_skj[, 6:ncol(ord_df_skj)])[1] * k.site
    )),
    nrow = dim(ord_df_skj[, 6:ncol(ord_df_skj)])[1]),
    k = k.site,
    model = "global",
    maxit = 200,
    smin = 1e-7,
    sfgrmin = 1e-7
  )
}
for (i in 1:100) {
  mds_ulv[[i]] <- monoMDS(
    geodist.y.ulv,
    matrix(c(runif(
      dim(ord_df_ulv[, 6:ncol(ord_df_ulv)])[1] * k.site
    )),
    nrow = dim(ord_df_ulv[, 6:ncol(ord_df_ulv)])[1]),
    k = k.site,
    model = "global",
    maxit = 200,
    smin = 1e-7,
    sfgrmin = 1e-7
  )
}
for (i in 1:100) {
  mds_lav[[i]] <- monoMDS(
    geodist.y.lav,
    matrix(c(runif(
      dim(ord_df_lav[, 6:ncol(ord_df_lav)])[1] * k.site
    )),
    nrow = dim(ord_df_lav[, 6:ncol(ord_df_lav)])[1]),
    k = k.site,
    model = "global",
    maxit = 200,
    smin = 1e-7,
    sfgrmin = 1e-7
  )
}
for (i in 1:100) {
  mds_gud[[i]] <- monoMDS(
    geodist.y.gud,
    matrix(c(runif(
      dim(ord_df_gud[, 6:ncol(ord_df_gud)])[1] * k.site
    )),
    nrow = dim(ord_df_gud[, 6:ncol(ord_df_gud)])[1]),
    k = k.site,
    model = "global",
    maxit = 200,
    smin = 1e-7,
    sfgrmin = 1e-7
  )
}

# save/overwrite or load mds object
saveRDS(mds, "../../results/models/mds_k3.Rds")
saveRDS(mds_skj, "../../results/models/mds_k2_skj.Rds")
saveRDS(mds_ulv, "../../results/models/mds_k2_ulv.Rds")
saveRDS(mds_lav, "../../results/models/mds_k2_lav.Rds")
saveRDS(mds_gud, "../../results/models/mds_k2_gud.Rds")

mds <- readRDS("../../results/models/mds_k3.Rds")
mds_skj <- readRDS("../../results/models/mds_k2_skj.Rds")
mds_ulv <- readRDS("../../results/models/mds_k2_ulv.Rds")
mds_lav <- readRDS("../../results/models/mds_k2_lav.Rds")
mds_gud <- readRDS("../../results/models/mds_k2_gud.Rds")

# Extract the stress values as a vector.
# Stress values are provided by the 22th element in each "subobject list"
# as mismatch between the rank order of distances in the data,
# and the rank order of distances in the ordination
mds.stress <- unlist(lapply(mds, function(v) {
  v[[22]]
}))
{
  mds.stress.skj <- unlist(lapply(mds_skj, function(v) {
    v[[22]]
  }))
  mds.stress.ulv <- unlist(lapply(mds_ulv, function(v) {
    v[[22]]
  }))
  mds.stress.lav <- unlist(lapply(mds_lav, function(v) {
    v[[22]]
  }))
  mds.stress.gud <- unlist(lapply(mds_gud, function(v) {
    v[[22]]
  }))
}

mds.stress.skj <- unlist(lapply(mds_skj,function(v){v[[22]]}))
mds.stress.ulv <- unlist(lapply(mds_ulv,function(v){v[[22]]}))
mds.stress.lav <- unlist(lapply(mds_lav,function(v){v[[22]]}))
mds.stress.gud <- unlist(lapply(mds_gud,function(v){v[[22]]}))

# view and order the stress values for the 100 MDSs:
mds.stress
ordered <- order(mds.stress)
{
  ordered.skj <- order(mds.stress.skj)
  ordered.ulv <- order(mds.stress.ulv)
  ordered.lav <- order(mds.stress.lav)
  ordered.gud <- order(mds.stress.gud)
}

# get stress values for the solutions with the lowest and 2nd lowest stress
mds.stress[ordered[1]] # these two should be similar and small
mds.stress[ordered[2]]

mds.stress.skj[ordered.skj[1]]
mds.stress.skj[ordered.skj[2]]
mds.stress.ulv[ordered.ulv[1]] # 0.271 (lowest)
mds.stress.ulv[ordered.ulv[2]]
mds.stress.lav[ordered.lav[1]]
mds.stress.lav[ordered.lav[2]] # 0.310 (highest)
mds.stress.gud[ordered.gud[1]]
mds.stress.gud[ordered.gud[2]]

# scale axes to half-change units and perform a varimax rotation by postMDS
mds.best <- postMDS(
  mds[[ordered[1]]],
  geodist.y,
  pc = TRUE,
  halfchange = TRUE,
  threshold = 0.8
)
mds.2best <- postMDS(
  mds[[ordered[2]]],
  geodist.y,
  pc = TRUE,
  halfchange = TRUE,
  threshold = 0.8
)
{
  mds.best.skj <- postMDS(
    mds_skj[[ordered.skj[1]]],
    geodist.y.skj,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.2best.skj <- postMDS(
    mds_skj[[ordered.skj[2]]],
    geodist.y.skj,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.best.ulv <- postMDS(
    mds_ulv[[ordered.ulv[1]]],
    geodist.y.ulv,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.2best.ulv <- postMDS(
    mds_ulv[[ordered.ulv[2]]],
    geodist.y.ulv,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.best.lav <- postMDS(
    mds_lav[[ordered.lav[1]]],
    geodist.y.lav,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.2best.lav <- postMDS(
    mds_lav[[ordered.lav[2]]],
    geodist.y.lav,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.best.gud <- postMDS(
    mds_gud[[ordered.gud[1]]],
    geodist.y.gud,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
  mds.2best.gud <- postMDS(
    mds_gud[[ordered.gud[2]]],
    geodist.y.gud,
    pc = TRUE,
    halfchange = TRUE,
    threshold = 0.8
  )
}

# Procrustes comparisons
procrustes(mds.best, mds.2best, permutations = 999) # Procrustes sum of squares
# Procrustes sum of squares: 21.35
protest(mds.best, mds.2best, permutations = 999)
# Procrustes Sum of Squares (m12 squared):        0.01034
# Correlation in a symmetric Procrustes rotation: 0.9948
# Significance:  0.001
# OK, the two best solutions are similar like they should be

procrustes(mds.best.skj, mds.2best.skj, permutations = 999) # 46.17
protest(mds.best.skj, mds.2best.skj, permutations = 999) # *
procrustes(mds.best.ulv, mds.2best.ulv, permutations = 999) # 5.278
protest(mds.best.ulv, mds.2best.ulv, permutations = 999) # *
procrustes(mds.best.lav, mds.2best.lav, permutations = 999) # 2.362
protest(mds.best.lav, mds.2best.lav, permutations = 999) # *
procrustes(mds.best.gud, mds.2best.gud, permutations = 999) # 7.27
protest(mds.best.gud, mds.2best.gud, permutations = 999) # *

# Procrustes plot
plot(procrustes(mds.best, mds.2best, permutations = 999))
{
  par(mfrow = c(2, 2))
  plot(procrustes(mds.best.skj, mds.2best.skj, permutations = 999),
       main = "skj")
  plot(procrustes(mds.best.ulv, mds.2best.ulv, permutations = 999),
       main = "ulv")
  plot(procrustes(mds.best.lav, mds.2best.lav, permutations = 999),
       main = "lav")
  plot(procrustes(mds.best.gud, mds.2best.gud, permutations = 999),
       main = "gud")
}

# save best mds objects (site-specific only, global to be added)
mds_best_list = list(gud = mds.best.gud, 
                     lav = mds.best.lav, 
                     skj = mds.best.skj, 
                     ulv = mds.best.ulv)
for (i in 1:4) {
  saveRDS(mds_best_list[[i]],paste("../../results/models/mds_best_",names(mds_best_list[i]),".Rds", sep = ""))
}

# extract axes from lowest-stress mds
# global
# # k = 2
#  gnmds2_1 <- mds.best$points[,1]
#  gnmds2_2 <- mds.best$points[,2]
 # k = 3
gnmds3_1 <- mds.best$points[, 1]
gnmds3_2 <- mds.best$points[, 2]
gnmds3_3 <- mds.best$points[, 3]
# #  k = 4
# gnmds4_1 <- mds.best$points[,1]
# gnmds4_2 <- mds.best$points[,2]
# gnmds4_3 <- mds.best$points[,3]
# gnmds4_4 <- mds.best$points[,4]

# site-specific
mds_axes <-
  data.frame(
    site = c(
      rep("skj", 2 * length(mds.best.skj$points[, 1])),
      rep("ulv", 2 * length(mds.best.ulv$points[, 1])),
      rep("lav", 2 * length(mds.best.lav$points[, 1])),
      rep("gud", 2 * length(mds.best.gud$points[, 1]))
    ),
    axis_no = c(
      rep(1:2, each = length(mds.best.skj$points[, 1])),
      rep(1:2, each = length(mds.best.ulv$points[, 1])),
      rep(1:2, each = length(mds.best.lav$points[, 1])),
      rep(1:2, each = length(mds.best.gud$points[, 1]))
    ),
    ax_row_no = c(
      rep(1:length(mds.best.skj$points[, 1]), 2),
      rep(1:length(mds.best.ulv$points[, 1]), 2),
      rep(1:length(mds.best.lav$points[, 1]), 2),
      rep(1:length(mds.best.gud$points[, 1]), 2)
    ),
    axis_pts = c(
      gnmds_skj_2_1 = mds.best.skj$points[, 1],
      gnmds_skj_2_2 = mds.best.skj$points[, 2],
      gnmds_ulv_2_1 = mds.best.ulv$points[, 1],
      gnmds_ulv_2_2 = mds.best.ulv$points[, 2],
      gnmds_lav_2_1 = mds.best.lav$points[, 1],
      gnmds_lav_2_2 = mds.best.lav$points[, 2],
      gnmds_gud_2_1 = mds.best.gud$points[, 1],
      gnmds_gud_2_2 = mds.best.gud$points[, 2]
    )
  )

# save axes
saveRDS(gnmds3_1, "../../results/models/ordination/gnmds3_1.Rds")
saveRDS(gnmds3_2, "../../results/models/ordination/gnmds3_2.Rds")
saveRDS(gnmds3_3, "../../results/models/ordination/gnmds3_3.Rds")
write.csv(mds_axes,
          "../../results/models/ordination/gnmds_axes_k2_sitespecific.csv")

# alternative wide format
mds_obj_list <- mds_axes %>%
  select(site, axis_no, ax_row_no, axis_pts) %>%
  pivot_wider(names_from = c(site, axis_no),
              values_from = axis_pts)
write.csv(
  mds_obj_list,
  "../../results/models/ordination/gnmds_axes_k2_sitespecific_wide.csv",
  row.names = FALSE
)

# read axis objects
gnmds3_1 <- readRDS("../../results/models/gnmds3_1.Rds")
gnmds3_2 <- readRDS("../../results/models/gnmds3_2.Rds")
gnmds3_3 <- readRDS("../../results/models/gnmds3_3.Rds")
mds_axes <-
  read.csv("../../results/models/ordination/gnmds_axes_k2_sitespecific.csv")

# plot axes against each other
plot(gnmds3_1, gnmds3_2)
plot(gnmds3_1, gnmds3_3)

sites = c("skj", "ulv", "lav", "gud")
plot(
  mds_axes[mds_axes$axis_no == 1, ]$axis_pts,
  mds_axes[mds_axes$axis_no == 2, ]$axis_pts,
  main = "gnmds plot scores, axes 1 and 2",
  xlab = "gnmds axis 1",
  ylab = "gnmds axis 2"
)
for (j in sites) {
  abline(lm(mds_axes[mds_axes$site == j |
                       mds_axes$axis_no == 1, ]$axis_pts ~
              mds_axes[mds_axes$site == j |
                         mds_axes$axis_no == 2, ]$axis_pts))
}

# 4. CORRELATE DCA & GNMDS
# ------------------------------
# calculate Kendall"s Tau non-parametric rank correlation coefficients
dca <- readRDS("../../results/models/dca_global.Rds")
# global ordination (across all sites)
# Following Liu et al. (2008; <DOI>),
# we commonly regard a tau = 0.4 as a minimum for claiming that
# two axes express more or less the same core of variation.
# Preliminary mds run with k=4 gave cor.test(dca1,gnmds4_1,method="k") result
# tau = 0.8068197, p < 2.2e-16. Other axes had very low tau.
# Preliminary mds run with k=2 gave two "strong" correlations:
# dca1,gnmds2_1 tau=-0.7821  p<2.2e-16
# dca3,gnmds2_2 tau=0.3996  p<2.2e-16
# but several others were close.
# The final mds fitting with k=3 seems like the most appropriate:
cor.test(dca1, gnmds3_1, method = "k") # tau=-0.818   p<2.2e-16 *
cor.test(dca1, gnmds3_2, method = "k") # tau=-0.0253   p=0.00979
cor.test(dca1, gnmds3_3, method = "k") # tau=-0.0296   p=0.002535

cor.test(dca2, gnmds3_1, method = "k") # tau=-0.200  p<2.2e-16
cor.test(dca2, gnmds3_2, method = "k") # tau=0.000512  p=0.958
cor.test(dca2, gnmds3_3, method = "k") # tau=0.635   p<2.2e-16 *

cor.test(dca3, gnmds3_1, method = "k") # tau=-0.0658  p=1.751e-11
cor.test(dca3, gnmds3_2, method = "k") # tau=0.474   p<2.2e-16 *
cor.test(dca3, gnmds3_3, method = "k") # tau=0.0479   p=1.014e-06

cor.test(dca4, gnmds3_1, method = "k") # tau=-0.003   p=0.759
cor.test(dca4, gnmds3_2, method = "k") # tau=0.193    p<2.2e-16
cor.test(dca4, gnmds3_3, method = "k") # tau=0.0814   p<2.2e-16

plot(dca1, gnmds3_1)
plot(dca2, gnmds3_3)
plot(dca3, gnmds3_2)
# Both DCA and GNMDS pick up the same 3 axes.
# DCA axis 2 corresponds to GNMDS axis 3 and vice versa.
# Because DCA2 shows an artifact, we choose the nmds axes for further analyses

# site-specific correlations
# is k=3 still appropriate?
mds_obj_list <-
  as.data.frame(read.csv("../../data_processed/mds_k2_plotscore_axes.csv"))
# SKJELLINGAHAUGEN
for (i in 1:4) {
  # i denotes dca columns
  for (j in 2:3) {
    # j for mds columns
    correlations <- cor.test(dca_obj_list[[i]],
                             mds_obj_list[, j],
                             method = "k")
    out <- paste(
      "Kendall's rank correlation tau",
      "dca_ax_col:",
      i,
      "mds_ax_col:",
      j,
      "z:",
      correlations$statistic,
      "p:",
      correlations$p.value,
      "tau:",
      correlations$estimate,
      sep = ","
    )
    write(out,
          file = "../../results/cda_mds_k2_cor_skj.txt",
          append = TRUE)
  }
}
# ULVEHAUGEN
for (i in 5:8) {
  # i denotes dca columns
  for (j in 4:5) {
    # j for mds columns
    correlations <- cor.test(dca_obj_list[[i]],
                             na.omit(mds_obj_list[, j]),
                             method = "k")
    out <- paste(
      "Kendall's rank correlation tau",
      "dca_ax_col:",
      i,
      "mds_ax_col:",
      j,
      "z:",
      correlations$statistic,
      "p:",
      correlations$p.value,
      "tau:",
      correlations$estimate,
      sep = ","
    )
    write(out,
          file = "../../results/cda_mds_k2_cor_ulv.txt",
          append = TRUE)
  }
}
# LAVISDALEN
for (i in 9:12) {
  # i denotes dca columns
  for (j in 6:7) {
    # j for mds columns
    correlations <- cor.test(dca_obj_list[[i]],
                             na.omit(mds_obj_list[, j]),
                             method = "k")
    out <- paste(
      "Kendall's rank correlation tau",
      "dca_ax_col:",
      i,
      "mds_ax_col:",
      j,
      "z:",
      correlations$statistic,
      "p:",
      correlations$p.value,
      "tau:",
      correlations$estimate,
      sep = ","
    )
    write(out,
          file = "../../results/cda_mds_k2_cor_lav.txt",
          append = TRUE)
  }
}
# GUDMEDALEN
for (i in 13:16) {
  # i denotes dca columns
  for (j in 8:9) {
    # j for mds columns
    correlations <- cor.test(dca_obj_list[[i]],
                             na.omit(mds_obj_list[, j]),
                             method = "k")
    out <- paste(
      "Kendall's rank correlation tau",
      "dca_ax_col:",
      i,
      "mds_ax_col:",
      j,
      "z:",
      correlations$statistic,
      "p:",
      correlations$p.value,
      "tau:",
      correlations$estimate,
      sep = ","
    )
    write(out,
          file = "../../results/cda_mds_k2_cor_gud.txt",
          append = TRUE)
  }
}

# gnmds species scores calculated in separate script
# further analysis of axes in gnmds_analyses_species.R and gnmds_analyses_clim.R

# --------------------------------------------------
# sources/inspiration:
# Liu et al. 2008 Sommerfeltia
