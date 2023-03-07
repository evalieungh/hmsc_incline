####################################
#  plot site-specific ordinations  #
#  with species scores, 
#  environmental data, 
#  and traits
####################################

# script by Eva Lieungh
# started 2023-01-16

library(tidyverse)
library(vegan)


# read data 
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/src/plotting/')

ord_df_list <- readRDS("../../data_processed/ordination_df_sitespecific_list.Rds")

species_scores_list <- readRDS("../../results/gnmds_k2_species_scores_sitespecific.Rds")

occurrence_vector_list <- readRDS("../../data_processed/occurrence_sums_sitespecific_vector_list.Rds")

sites = c("skj", "ulv", "lav", "gud")

# plot site-specific subplot scores and species scores
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
      colour = "Number of species\npresent in subplot",
      caption = paste("Points: subplot site scores. Blue arrow: envfit vector of number of species per subplot, r = ",
                      round(occurrence_vector_list[[site]]$vectors$r, digits = 2))
    ) +
    # add site scores for subplots, 
    # colour them by sum of occurrences
    geom_point(aes(colour = ord_df_list[[site]]$occurrences),
               size = 1) +
    scale_colour_gradient(
      low = "yellow",
      high = "red"
    ) +
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
         path = "../../results/figures/",
         device = "png", 
         bg = "white",
         dpi = "print",
         width = 5,
         height = 5,
         scale = 2)
}
