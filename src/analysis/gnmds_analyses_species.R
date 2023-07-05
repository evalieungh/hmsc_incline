#####################################################
#    gnmds axes analysis: co-occurrences, traits    #
#####################################################

# Script by Eva L
# started 2023-01-16

{library(tidyverse) # formatting
library(vegan) # gradient analysis
library(graphics)  # identify points on ordination diagrams
library(plotly)} # for 3D plotting

# 1. species scores global
# 2. species scores vs number of species in subplot
# 3. sla, height, leaf area

# This script...
# formats data a bit, 
# plots plot and species scores
# with occurrence sums and envfit vector
# (...)

# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/')
  # global ordination with k = 3 (three-dimensional solution)
mds <- readRDS('results/models/mds_k3.Rds') 

gnmds3_1 <- readRDS('results/models/ordination/gnmds3_1.Rds') # axis 1
gnmds3_2 <- readRDS('results/models/ordination/gnmds3_2.Rds') # axis 2
gnmds3_3 <- readRDS('results/models/ordination/gnmds3_3.Rds')

species_scores_global <-
  data.frame(
    mds1var = readRDS('results/models/ordination/k3_mds1var.Rds'),
    # species scores for axis 1
    mds2var = readRDS('results/models/ordination/k3_mds2var.Rds'),
    # species scores for axis 2
    mds3var = readRDS('results/models/ordination/k3_mds3var.Rds')
  )

ord_df <- readRDS('data_processed/ord_df.Rds') # data frame with ordination axes and species occurrences; to be modified further down
ord_df[1:5,1:10]

  # site-specific
ord_df_list <- readRDS("data_processed/ordination_df_sitespecific_list_occurrencesums.Rds")

species_scores_list <- readRDS("results/gnmds_k2_species_scores_sitespecific.Rds")

occurrence_vector_list <- readRDS("data_processed/occurrence_sums_sitespecific_vector_list.Rds")

sites = c("skj", "ulv", "lav", "gud")



# SPECIES SCORES
# ------------------------------
# compare relative gnmds species scores
  # global
plot_list = list()
for (i in 2:3) {
  gnmds_number = paste("mds", i, sep = "")
  y_axis = ord_df[, gnmds_number]
  if (i == 2) {
    mds_var = mds2var
  } else {
    mds_var = mds3var
  }
  plot_list[[i]] <-
    ggplot(ord_df, aes(x = mds1,
                       y = y_axis)) +
    labs(
      title = paste("global GNMDS relative species scores"),
      x = "gnmds axis 1",
      y = paste("gnmds axis", i)
    ) +
    geom_point(color = "grey", size = 0.7) +
    ggrepel::geom_text_repel(
      data = species_scores_global,
      mapping = aes(x = mds1var,
                    y = mds_var),
      label = rownames(species_scores_global),
      check_overlap = FALSE,
      size = 3
    ) +
    theme_minimal()
  #print(plot_list[[i]])
  ggsave(
    filename = paste("species_plot_global_1", i, ".png", sep = ""),
    plot = plot_list[[i]],
    path = "results/figures/",
    device = "png",
    bg = "white",
    dpi = "print",
    width = 5,
    height = 5,
    scale = 2
  )
}

# site-specific
plot_list = list()
for (site in sites) {
  print(paste("plotting for site:", site))
  sitespecific_df = as.data.frame(species_scores_list[[site]])
  speciesnames = rownames(sitespecific_df)
  vector_df = as.data.frame(scores(occurrence_vector_list[[site]], display = "vectors"))
  # start plotting
  plot_list[[site]] <-
    ggplot(ord_df_list[[site]],
           aes(
             x = unlist(ord_df_list[[site]]$mds1),
             y = unlist(ord_df_list[[site]]$mds2)
           )) +
    # add labels
    labs(
      title = paste("GNMDS relative species scores:", toupper(site)),
      x = "gnmds axis 1",
      y = "gnmds axis 2",
      colour = "number of species\n present in subplot",
      caption = paste("Blue arrow: envfit vector of number of species per subplot, r = ",
                      round(occurrence_vector_list[[site]]$vectors$r, digits = 2))
    ) +
    # # add site scores for subplots, 
    # # colour them by sum of occurrences
    # geom_point(aes(colour = ord_df_list[[site]]$occurrences),
    #            size = 1) +
    # scale_colour_gradient(
    #   low = "yellow",
    #   high = "red"
    # ) +
    # add species labels
    ggrepel::geom_text_repel(
      data = sitespecific_df,
      mapping = aes(x = mds1,
                    y = mds2),
      label = speciesnames
    ) + 
    # add vector
    geom_segment(data = vector_df,
                 aes(x = 0, xend = vector_df$MDS1,
                     y = 0, yend = vector_df$MDS2),
                 arrow = arrow(length = unit(0.5, "mm",),
                               type = "closed"), 
                 colour = "blue",
                 size = 1.5) +
    # # add label to vector
    # geom_text(aes(x = vector_df$MDS1,
    #               y = vector_df$MDS2,
    #               label = round(occurrence_vector_list[[site]]$vectors$r, digits = 2)),
    #           colour = "blue",
    #           fill = "white",
    #           size = 4) +
    coord_fixed() +
    theme_minimal()
  # print/show the plot in R
  # print(plot_list[[site]])
  # save the plot
  ggsave(filename = paste("species_plot_", site, ".png", sep=""),
         plot = plot_list[[site]],
         path = "results/figures/",
         device = "png", 
         bg = "white",
         dpi = "print",
         width = 5,
         height = 5,
         scale = 2)
}


# 3. SLA, HEIGHT, LEAF AREA - NB needs updating!
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
# get gnmds species score vs species score differences, test correlation to omegas
omegas_l <- readRDS('data_processed/omegas_long.Rds') # from format_omegas.R
mds1var <- readRDS('results/models/k3_mds1var.Rds') # species scores for axis 1
mds2var <- readRDS('results/models/k3_mds2var.Rds')
mds3var <- readRDS('results/models/k3_mds3var.Rds')

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