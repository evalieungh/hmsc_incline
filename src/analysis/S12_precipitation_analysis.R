##############################
#    precipitation levels    #
##############################

# compare species associations across precipitation levels

# association matrices sum to 0, therefore site means should not be very informative. Need to figure out some meaningful way to compare. 

library(tidyverse)

# read/make data
sites = c('skj','gud', 'lav', 'ulv')
prec = c(2725,1925,1321,593)
a_skj <- read.csv('Data/model_output/Omega_Skjellingahaugen_subplot.csv', row.names = 1)
a_gud <- read.csv('Data/model_output/Omega_Gudmedalen_subplot.csv',row.names = 1) # estimated associations
a_lav <- read.csv('Data/model_output/Omega_Lavisdalen_subplot.csv',row.names = 1)
a_ulv <- read.csv('Data/model_output/Omega_Ulvehaugen_subplot.csv',row.names = 1)


# Are mean interactions different across sites?
#-------------------------------------------------------------------
a_sitemeans <- data.frame(sites = sites, a_mean = c(rep(0,4)), prec = prec)
sp.means.skj <- colMeans(a_skj[sapply(a_skj, is.numeric)])
a_sitemeans$a_mean[1] = mean(sp.means.skj)

sp.means.gud <- colMeans(a_gud[sapply(a_gud, is.numeric)])
a_sitemeans$a_mean[2] = mean(sp.means.gud)

sp.means.lav <- colMeans(a_lav[sapply(a_lav, is.numeric)])
a_sitemeans$a_mean[3] = mean(sp.means.lav)

sp.means.ulv <- colMeans(a_ulv[sapply(a_ulv, is.numeric)])
a_sitemeans$a_mean[4] = mean(sp.means.ulv)

plot(a_sitemeans$prec,a_sitemeans$a_mean) # no clear trend?

# are species-specific mean associations different across sites? 
#-------------------------------------------------------------------
sp.means <- data.frame(skj = c(sp.means.skj), gud = c(sp.means.gud),
                       lav = c(sp.means.lav), ulv = c(sp.means.ulv))
# {plot(rep(prec[1],nrow(sp.means)),sp.means$sp.means.skj,
#      xlim = c(500,2900),ylim = c(min(sp.means),max(sp.means)),
#      xlab = 'Ulv     Lav      Gud      Skj', ylab = 'species mean associations',
#      col = rainbow(58), pch = 16)
# points(rep(prec[2],nrow(sp.means)),sp.means$sp.means.gud,col = rainbow(58), pch = 16)
# points(rep(prec[3],nrow(sp.means)),sp.means$sp.means.lav,col = rainbow(58), pch = 16)
# points(rep(prec[4],nrow(sp.means)),sp.means$sp.means.ulv,col = rainbow(58), pch = 16)
# abline(h=0)
# }

library(ggplot2)

df <- sp.means %>% 
  add_column(species = as.factor(Species$species)) %>% 
  pivot_longer(-species,)

ggplot(data = df,
       aes(x = c(rep(prec, nrow(sp.means))), y = value, 
           color = species)) +
  geom_point() +
  geom_line(data = df,
            aes(x = c(rep(prec, nrow(sp.means))), y = value,  
                color = species)) +
  ggtitle('precipitation vs species mean associations') +
  xlab('precipitation / sites Ulv, Lav, Gud, Skj') +
  ylab('per species mean co-occurrence with other species')

# split graminoids/others
grams <- gram[gram$graminoid==1,]
grams <- grams$species

forbs <- gram[gram$graminoid==0,]
forbs <- forbs$species
