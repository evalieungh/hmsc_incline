###############################################
#  plot relationships between species traits  # 
#      and pairwise co-occurrence scores      #
###############################################

# Script by Eva Lieungh, Ryan Burner
# started 2023-01-26

# this scripts relies on data made with fig3_omega_trait_model_data.R

# make a plot, similar to Fig 4 in Burner et al. 2021
# https://onlinelibrary.wiley.com/doi/epdf/10.1111/jbi.14272, 
# https://onlinelibrary.wiley.com/cms/asset/4a44d384-37dc-426b-b162-3b2684f3537f/jbi14272-fig-0004-m.png
# as a collection of plots per trait, with site on y axis,
# and relationships between species traits and pairwise co-occurrence scores
# as bars with confidence intervals along x axis.

library(tidyverse)
library(ggplot2)
library(cowplot)
library(patchwork)

# read data
# --------------------------------
setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")

data <- read.csv("data_processed/fig3_omegas_traits_data.csv")
modeldata <- readRDS("data_processed/fig3_omega-trait_model_data.Rds")

# set labels for plotting
labels = c(
  height_mm_diff = "Height",
  fresh_mass_g_diff = "Fresh mass",
  dry_mass_g_diff = "Dry mass",
  leaf_area_cm2_diff = "Leaf area",
  SLA_cm2_g_diff = "SLA",
  LDMC_g_g_diff = "LDMC",
  leaf_thickness_diff = "Leaf thickness",
  N_percent_diff = "N percent",
  C_percent_diff = "C percent",
  CN_ratio_diff = "CN ratio"
)

# set order of sites so they appear from driest to wettest
modeldata$siteID <- factor(
  modeldata$siteID,
  levels = c("Ulvehaugen",
             "Lavisdalen",
             "Gudmedalen",
             "Skjellingahaugen")
)

# not sure whether to use this: 
# split data into two groups to make x axis more readable in plots
morphological_traits = c("height_mm_diff",
                         "fresh_mass_g_diff",
                         "dry_mass_g_diff",
                         "leaf_area_cm2_diff",
                         "SLA_cm2_g_diff",
                         "LDMC_g_g_diff",
                         "leaf_thickness_diff")

modeldata_morphological <-
  modeldata[modeldata$Var1 %in% morphological_traits, ]

chemical_traits = c("N_percent_diff",
                    "C_percent_diff",
                    "CN_ratio_diff")

modeldata_chemical <-
  modeldata[modeldata$Var1 %in% chemical_traits, ]

# make plot
# --------------------------------
# Significance level set to 95 %, but can be changed by changing code below.
# The x axis shows the model coefficient for the trait's influence on the omegas
# for the given site.
fig3 <-
  ggplot(modeldata[which(modeldata$Var1 != "(Intercept)"),],
         aes(
           x = Coefficient,
           y = siteID,
           xmin = min95,
           xmax = max95,
           color = Sig95
         )) + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "white", fill = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))

fig3 +
  facet_grid(Var1 ~ ., 
             labeller = labeller(Var1 = labels)) +
  geom_pointrange(
    color = "black",
    size = 2,
    fatten = 0.03,
    aes(
      x = Coefficient,
      y = siteID,
      xmin = min95,
      xmax = max95
    )
  ) +
  # geom_pointrange(
  #   color = "grey74",
  #   size = 3,
  #   fatten = 0.03,
  #   aes(
  #     x = Coefficient,
  #     y = siteID,
  #     xmin = Sim50min,
  #     xmax = Sim50max
  #   )
  # ) +
  geom_pointrange(color = 
                    modeldata$Sig95[which(modeldata$Var1 != "(Intercept)")], 
                  size = 0.75
                  ) +
  geom_vline(xintercept = 0,
             color = "black",
             size = 0.5) + 
  xlab("Regression parameter") +
  ylab("Site, from wettest (Skjellingahaugen) to driest (Ulveaugen)")

# make similar plot, but split into morphological and chemical traits
# --------------------------------------------------------------------
fig3a <-
  ggplot(modeldata_morphological,
         aes(
           x = Coefficient,
           y = siteID,
           xmin = min95,
           xmax = max95,
           color = Sig95
         )) + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))

(
  fig3a <- fig3a +
    facet_grid(Var1 ~ .,
               labeller = labeller(Var1 = labels)) +
    geom_pointrange(
      color = "grey50",
      size = 2,
      fatten = 0.03,
      aes(
        x = Coefficient,
        y = siteID,
        xmin = min95,
        xmax = max95
      )
    ) +
    # geom_pointrange(
    #   color = "grey74",
    #   size = 3,
    #   fatten = 0.03,
    #   aes(
    #     x = Coefficient,
    #     y = siteID,
    #     xmin = Sim50min,
    #     xmax = Sim50max
    #   )
    # ) +
  geom_pointrange(color = modeldata_morphological$Sig95,
                  size = 0.75) +
    geom_vline(
      xintercept = 0,
      color = "black",
      size = 0.5
    ) +
    xlab("Regression parameter") +
    ylab("")
)


# ---------------

fig3b <-
  ggplot(modeldata_chemical,
         aes(
           x = Coefficient,
           y = siteID,
           xmin = min95,
           xmax = max95,
           color = Sig95
         )) + 
  theme_light() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_blank(),
        strip.background = element_rect(colour = "black", fill = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"))

(fig3b <- fig3b +
  facet_grid(Var1 ~ ., 
             labeller = labeller(Var1 = labels)) +
  geom_pointrange(
    color = "grey87",
    size = 2,
    fatten = 0.03,
    aes(
      x = Coefficient,
      y = siteID,
      xmin = min95,
      xmax = max95
    )
  ) +
  # geom_pointrange(
  #   color = "grey74",
  #   size = 3,
  #   fatten = 0.03,
  #   aes(
  #     x = Coefficient,
  #     y = siteID,
  #     xmin = Sim50min,
  #     xmax = Sim50max
  #   )
  # ) +
geom_pointrange(color = modeldata_chemical$Sig90, 
                size = 0.75
) +
  geom_vline(xintercept = 0,
             color = "black",
             size = 0.5) + 
  xlab("Regression parameter") +
  ylab("")
)


# combine morphological and chemical panels into one figure
# ------------------------------------------------------------

(fig3_combined <- plot_grid(fig3a, 
                           fig3b,
                           labels = c('a', 'b'),
                           label_size = 10,
                           nrow = 2, ncol = 1,
                           rel_heights = c(7, 3)))

ggsave(filename = "results/figures/figure_3.png",
       plot = fig3_combined,
       device = "png", 
       bg = "white",
       dpi = 300,
       width = 3,
       height = 7,
       scale = 1.5)

ggsave(filename = "results/figures/figure_3.pdf",
       plot = fig3_combined,
       device = "pdf",
       width = 3,
       height = 7,
       scale = 1.5)  
  
  
  
  
