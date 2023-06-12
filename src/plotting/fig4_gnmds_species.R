#############################################
#  site-specific gnmds species score plots  #
#############################################

# script by Eva Lieungh, Rune Halvorsen, Michal Torma
# started 2023-06-08

library(tidyverse)
library(vegan)

# create panels for figure 4
# plot site-specific subplot scores and selected species scores
# colour subplots by number of species present

# read data 
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/')

ord_df_list <- readRDS("data_processed/ordination_df_sitespecific_list_occurrencesums.Rds")
ord_df_list[[1]][1:5,1:10]

species_scores_list <- readRDS("results/gnmds_k2_species_scores_sitespecific.Rds")
str(species_scores_list)

occurrence_vector_list <- readRDS("data_processed/occurrence_sums_sitespecific_vector_list.Rds")
str(occurrence_vector_list)

sites = c("skj", "ulv", "lav", "gud")

# change gnmds scores from list to double formats
for (site in sites) {
  ord_df_list[[site]]$mds1 <- unlist(ord_df_list[[site]]$mds1)
  ord_df_list[[site]]$mds2 <- unlist(ord_df_list[[site]]$mds2)
}

# Calculate the overall min and max values of the color scale
# as the min and max number of occurrences in a subplot
(overall_min <- min(unlist(lapply(ord_df_list, 
                                  function(df) min(df$occurrences)))))
(overall_max <- max(unlist(lapply(ord_df_list, 
                                  function(df) max(df$occurrences)))))

# define some species for text labeling
species = c("Ver_alp",
            "Sib_pro",
            "Vio_bif")

# make plots in loop over the four sites
plot_list = list()
legends = list()

for (site in sites) {
  print(paste("plotting for site:", site))
  # define a subset of species to label with text 
  sitespecific_df = as.data.frame(species_scores_list[[site]])
  species_subset = subset(sitespecific_df, 
                          rownames(sitespecific_df) %in% species)
  speciesnames = rownames(species_subset)
  # define site names for plot titles
  sitename = c(ifelse(site == "skj", "Skjellingahaugen, 3402 mm",
                      ifelse(site == "gud", "Gudmedalen, 2130 mm",
                             ifelse(site == "lav", "Lavisdalen, 1561 mm",
                                    "Ulvehaugen, 1226 mm"))))
  # get vectors (arrows) to plot
  vector_df = as.data.frame(scores(occurrence_vector_list[[site]], display = "vectors"))
  
  # start plotting
  plot_list[[site]] <-
    ggplot(ord_df_list[[site]],
           aes(
             x = mds1,
             y = mds2
           )) +
    # add labels
    labs(
      title = sitename,
      x = "GNMDS axis 1",
      y = "GNMDS axis 2",
      colour = "Number of species\npresent in subplot",
      # add r (squared correlation coefficient of vector)
      caption = paste("r = ",
                      round(occurrence_vector_list[[site]]$vectors$r, 
                            digits = 2))
    ) +
    # add site scores for subplots, 
    # colour them by sum of occurrences
    geom_point(aes(
      colour = occurrences)) +
    scale_colour_gradient(
      low = "yellow", high = "darkred",
      limits = c(overall_min, overall_max)
    ) +
    # add species labels
    ggrepel::geom_text_repel(
      data = species_subset,
      mapping = aes(x = mds1,
                    y = mds2),
      label = speciesnames
    ) + 
    # add vector
    geom_segment(data = vector_df,
                 aes(x = 0, xend = MDS1,
                     y = 0, yend = MDS2),
                 arrow = arrow(length = unit(0.5, "mm",),
                               type = "closed"), 
                 colour = "black",
                 linewidth = 1) +
    # set some coordinate and ggplot parameters
    coord_fixed() +
    theme_minimal()
  # store the legend from each plot
  legends[[site]] <- get_legend(plot_list[[site]])
  
  # Remove the legend from individual plots
  plot_list[[site]] <- plot_list[[site]] +
    guides(colour = "none")

  # save the plot
  ggsave(filename = paste("species_plot_", site, ".png", sep=""),
         plot = plot_list[[site]],
         path = "results/figures/",
         device = "png", 
         bg = "white",
         dpi = 300,
         width = 5,
         height = 5,
         scale = 1.5)
}

# combine the four plots in a composite grid figure
# -----------------------------------------------------------
library(cowplot)
library(patchwork)

composite_plot <- plot_grid(plotlist = plot_list, 
                            labels = c('a', 'b', 'c', 'd'),
                            nrow = 2, ncol = 2,
                            rel_widths = c(1, 2),
                            align = "h")

# Add legend to the composite plot
(composite_plot_with_legend <- composite_plot +
  theme(legend.position = "top", 
        legend.justification = "right",
        legend.title.align = 0.5) +
  guides(colour = guide_legend(
    override.aes = list(title = "Number of species\npresent in subplot"))) +
  inset_element(legends[[1]], # all legends are the same, so just pick one
                left = 0.86, bottom = 0,
                right = 1, top = 1)) 

ggsave(filename = "results/figures/figure_4.png",
       plot = composite_plot_with_legend,
       device = "png", 
       bg = "white",
       dpi = 300,
       width = 5,
       height = 5,
       scale = 1.5)

ggsave(filename = "results/figures/figure_4.pdf",
       plot = composite_plot_with_legend,
       device = "pdf",
       scale = 1.5)

