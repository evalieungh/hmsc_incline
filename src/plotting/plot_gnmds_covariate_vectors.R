#####################################################
#  plot envfit vectors of covariates to gnmds axes  #
#####################################################

# Script by Eva Lieungh
# started 2023-06-19

# read base plots and environmental vectors from gnmds_analyses_clim.R
# and plot vectors on top of plot scores

# read data
setwd('C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/')

global_prec_plot_12 <- readRDS("data_processed/gnmds_global_prec_plot_12.Rds")
global_prec_plot_13 <- readRDS("data_processed/gnmds_global_prec_plot_13.Rds")

global_prec_vector_12 <- readRDS("results/models/envfit_global_12_precipitation.Rds")
global_prec_vector_13 <- readRDS("results/models/envfit_global_13_precipitation.Rds")

global_soilmst_vector_12 <-
  readRDS("results/models/ordination/envfit_global_12_soilmst.Rds")
global_soilmst_vector_13 <-
  readRDS("results/models/ordination/envfit_global_13_soilmst.Rds")

global_soiltmp_vector_12 <-
  readRDS("results/models/ordination/envfit_global_12_soiltmp.Rds")
global_soiltmp_vector_13 <-
  readRDS("results/models/ordination/envfit_global_13_soiltmp.Rds")

vectors_12 <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  Covariate = c("Precipitation",
                "Soil moisture",
                "Soil temperature"),
  xend = c(
    global_prec_vector_12$mds1,
    global_soilmst_vector_12$mds1,
    global_soiltmp_vector_12$mds1
  ),
  yend = c(
    global_prec_vector_12$mds2,
    global_soilmst_vector_12$mds2,
    global_soiltmp_vector_12$mds2
  )
)

vectors_13 <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  Covariate = c("Precipitation",
                "Soil moisture",
                "Soil temperature"),
  xend = c(
    global_prec_vector_13$mds1,
    global_soilmst_vector_13$mds1,
    global_soiltmp_vector_13$mds1
  ),
  yend = c(
    global_prec_vector_13$mds3,
    global_soilmst_vector_13$mds3,
    global_soiltmp_vector_13$mds3
  )
)

# plot
(plot_a <- global_prec_plot_12 +
    geom_segment(
      data = vectors_12,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(length = unit(0.3, "cm")),
      linewidth = 0.8
    ) +
    geom_text(data = vectors_12,
              aes(
                x = xend + 0.05,
                y = yend - 0.05,
                label = Covariate
              ),
              size = 3)
)

label_y_positions = c(-0.3, -0.39, -0.15)
label_x_positions = c(0.67, 0.19, 0.83)
(plot_b <- global_prec_plot_13 +
    geom_segment(
      data = vectors_13,
      aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      arrow = arrow(length = unit(0.3, "cm")),
      linewidth = 0.8
    ) +
    geom_text(data = vectors_13,
              aes(
                x = label_x_positions,
                y = label_y_positions,
                label = Covariate),
              size = 3)
)

# combine and save plots
(fig_S1 <- plot_grid(
  plot_a,
  plot_b,
  nrow = 2,
  labels = c('a', 'b'),
  align = "v"
))

ggsave(filename = "results/figures/figure_S1.png",
       plot = fig_S1,
       device = "png", 
       bg = "white",
       dpi = 300,
       width = 3,
       height = 4,
       scale = 1.5)
