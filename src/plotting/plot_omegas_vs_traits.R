###############################
#    plot omegas vs traits    #
###############################

# script by EL

library(tidyverse)

# read data
df <- readRDS('data/plotting_df.RData') # omegas, formatted in S9 format omegas script
maxtraitdf <- readRDS('data/trait_max.RData') # trait values are means of locally (site) measured max of 10 individuals (Ragnhild Gya's MSC), formatted in format_traits script. 

# join the data
dfOmTr <- left_join(df,maxtraitdf, by = c('sp1' = 'species'))

# subset only strong associations (both positive and negative)
dfOmTrSub <- dfOmTr[dfOmTr$assoc_strength!='weak',]

# simple plots of traits vs omegas, no apparent relationship
plot(dfOmTr$height,dfOmTr$association)
plot(dfOmTr$sla,dfOmTr$association)
plot(dfOmTr$leaf_area,dfOmTr$association)

plot(dfOmTrSub$height,dfOmTrSub$association)
plot(dfOmTrSub$sla,dfOmTrSub$association)
plot(dfOmTrSub$leaf_area,dfOmTrSub$association)


