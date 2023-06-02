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

# read data
# --------------------------------