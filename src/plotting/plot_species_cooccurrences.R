#########################################
#    plot species association ranges    #
#########################################

# script by EL

# following discussion with Vigdis, Joachim, Ragnhild, make a plot that shows the range, mean and sizes of all their co-occurrences with other species. Species on Y axis, co-occurrence coefficient on X axis.

# First sorted alphabetically so it's easy to find species, 
# Second ordered by summed co-occurrence coefficients
# Third ordered by species SLA

library(tidyverse) # for piping, forcats functions, and ggplot2

# read data (from S9 format omegas script)
df <- readRDS('data/plotting_df.RData')

# NB! this data is only for Skjellingahaugen now. Repeat for the rest later

# build the alphabetic plot
# ------------------------------------------------
(plot1 <- df %>%
  ggplot(aes(x=association, y=sp1, colour = assoc_strength)) + 
   # set labels
   labs(title = 'Summed relative species co-occurrences', caption = 'Each dot is a co-occurrence value with another species. Red triangles are summed values per species') +
   xlab('Co-occurrence') + ylab('Species') +
   # add points for all co-occurrence values
   geom_point(aes(colour = assoc_strength)) +
   scale_color_manual(values = c("blue","red", "grey")) +
   # add sums
   geom_point(aes(x= summed, y=sp1), colour = 'red', shape = 'triangle') 
)

# build the sum-ordered plot
# ------------------------------------------------
# sorting object sp_order_sum defined in S9 script
(plot2 <- df %>%
   # reorder the species following the summed co-occurrences
   mutate(sp1 = fct_relevel(sp1, sp_order_sum)) %>%
   ggplot(aes(x=association, y=sp1, colour = assoc_strength)) + 
   # set labels
   labs(title = 'Summed relative species co-occurrences', caption = 'Each black dot is a co-occurrence value with another species. Red triangles are summed values per species') +
   xlab('Co-occurrence') + ylab('Species') +
   # add points for all co-occurrence values
   geom_point(aes(colour = assoc_strength, alpha = 1/7)) +
   scale_color_manual(values = c("blue","red", "grey")) +
   # add sums
   geom_point(aes(x= summed, y=sp1), colour = 'red', shape = 'triangle') 
)

# build the SLA-ordered plot
# ------------------------------------------------
# sorting object sp_order_sla defined in 
(plot3 <- df %>%
   # reorder the species following the summed co-occurrences
   mutate(sp1 = fct_relevel(sp1, sp_order_sla)) %>%
   ggplot(aes(x=association, y=sp1, colour = assoc_strength)) + 
   # set labels
   labs(title = 'Summed relative species co-occurrences', caption = 'Each black dot is a co-occurrence value with another species. Red triangles are summed values per species') +
   xlab('Co-occurrence') + ylab('Species') +
   # add points for all co-occurrence values
   geom_point(aes(colour = assoc_strength, alpha = 1/7)) +
   scale_color_manual(values = c("blue","red", "grey")) +
   # add sums
   geom_point(aes(x= summed, y=sp1), colour = 'red', shape = 'triangle') 
)


# next: 
# sort species by trait values and plot again
# add the other sites
# 
