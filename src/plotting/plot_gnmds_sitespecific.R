####################################
#  plot site-specific ordinations  #
#  with species scores, 
#  environmental data, 
#  and traits
####################################

# script by Eva Lieungh
# started 2023-01-16

# almost identical script to the one for figure 4, but plots ALL species
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

# make plots in loop
plot_list = list()
for (site in sites) {
  print(paste("plotting for site:", site))
  sitespecific_df = as.data.frame(species_scores_list[[site]])
  speciesnames = rownames(sitespecific_df)
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
      colour = occurrences)) + # ERROR no such index at level 1???
    scale_colour_gradient(
      low = "yellow",
      high = "darkred"
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
                 aes(x = 0, xend = MDS1,
                     y = 0, yend = MDS2),
                 arrow = arrow(length = unit(0.5, "mm",),
                               type = "closed"), 
                 colour = "black",
                 linewidth = 1) +
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
  ggsave(filename = paste("all_species_plot_", site, ".png", sep=""),
         plot = plot_list[[site]],
         path = "results/figures/",
         device = "png", 
         bg = "white",
         dpi = 300,
         width = 5,
         height = 5,
         scale = 1.5)
}

