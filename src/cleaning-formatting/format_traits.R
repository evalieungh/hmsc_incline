##################
#   trait data   #
##################

# Script by EL

# clean and format trait data from SeedClim 2016 and 2017 measurements 
# (Ragnhild Gya MSC thesis data)
# Data stored on SeedClim OSF (Vandvik et al. in prep) as 
# SeedClim_Trait_data_2012_2016.csv

# 

library(tidyverse)

traits <- read.csv('../../data/traits/SeedClim_Trait_data_2012_2016.csv') # available trait data
Species <- read.csv('../../data/specieslist.csv') # list of species used in models
gram <- read.csv('../../data/graminoids.csv') # filled in from memory + googling

# To do?: find and add seed mass trait from some other source! Ragnhild suggests: Kew, TRY, BIEN eller Tundra Trait Team 

# Select only Ragnhild's data from 2016, 2017,
traits <- traits[traits$year==c(2016)|traits$year==c(2017),]
# ...and only INCLINE sites
inclinesites = c('Gudmedalen','Lavisdalen','Ulvehaugen','Skjelingahaugen')
traits <- traits[traits$siteID %in% inclinesites,]

# get species names in same format as Species list
traits$species <- gsub('[.]','_',traits$species) # replace . with _

# remove redundant columns and species
traits <- left_join(Species, traits %>% 
                      select(species,siteID,trait,value)) # keep all rows from species list but only rows from trait data where species match
traits <- left_join(traits, gram)

# remove redundant traits
unique(traits$trait) 
traitlist = c('height_mm','leaf_area_cm2','SLA_cm2_g')
traits <- traits[traits$trait %in% traitlist,]

# save data frame in long format (all traits in same column)
saveRDS(traits,'../../data_processed/traits_long.RData')

# widen data frame to get one column per trait
traitdf <- traits %>%
  group_by(trait) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = trait, values_from = value) %>%
  select(-row)

# get only local maximum trait value instead of all 10 measurements
maxtraitdf_local <- traits %>%
  pivot_wider(names_from = trait, values_from = value, values_fn = max)
saveRDS(maxtraitdf_local,'../../data_processed/trait_localmax.RData')

# get mean of local maxima per species
maxtraitdf <- maxtraitdf_local %>%
  select(species,height_mm:SLA_cm2_g) %>%
  group_by(species) %>%
  summarise(height = mean(height_mm), 
            leaf_area = mean(leaf_area_cm2),
            sla = mean(SLA_cm2_g))
saveRDS(maxtraitdf,'../../data_processed/trait_max.RData')

# split data on traits
#----------------------------------------
height <- traits %>%
  filter(trait=='height_mm') %>%
  select(species, siteID, height_mm = value)
height <- left_join(Species, height, by = "species")
height$siteID[is.na(height$siteID)] <- 'global'

leaf_area <- traits %>%
  filter(trait=='leaf_area_cm2') %>%
  select(species, siteID, leaf_area_cm2 = value)
leaf_area <- left_join(Species, leaf_area, by = "species")
leaf_area$siteID[is.na(leaf_area$siteID)] <- 'global'

sla <- traits %>%
  filter(trait=='SLA_cm2_g') %>%
  select(species, siteID, SLA_cm2_g = value)
sla <- left_join(Species, sla, by = "species")
sla$siteID[is.na(sla$siteID)] <- 'global'

# identify and fill in missing traits
#----------------------------------------
h.index = which(is.na(height$height_mm), arr.ind = TRUE)
height$species[h.index]

l.index = which(is.na(leaf_area$leaf_area_cm2), arr.ind = TRUE)
leaf_area$species[l.index]

# Make site-specific trait matrices?
#----------------------------------------
# Check for trait differences between sites
aggregate(height$height_mm, list(height$siteID), FUN=mean) # does not seem to vary with precipitaion level, but it is quite different among sites.
aggregate(leaf_area$leaf_area_cm2, list(leaf_area$siteID), FUN=mean) 
aggregate(sla$SLA_cm2_g, list(sla$siteID), FUN=mean) 


# get height per species
h.gud <- height[height$siteID=='Gudmedalen'|height$siteID=='global',] # site subset
h.gud <- aggregate(height$height_mm, list(height$species), FUN=mean) # species trait mean

M.h.gud <- abs(outer(h.gud$x, h.gud$x, "-"))
rownames(M.h.gud) <- Species$species -> colnames(M.h.gud)
 M.h.gud[is.na(M.h.gud)] <- mean(na.omit(h.gud$x)) # set NAs to mean until real traits are filled in

# get leaf_area per species 
l.gud <- leaf_area[leaf_area$siteID=='Gudmedalen'|leaf_area$siteID=='global',] # site subset
l.gud <- aggregate(leaf_area$leaf_area_cm2, list(leaf_area$species), FUN=mean) # species trait mean

M.l.gud <- abs(outer(l.gud$x, l.gud$x, "-"))
rownames(M.l.gud) <- Species$species -> colnames(M.l.gud)
M.l.gud[is.na(M.l.gud)] <- mean(na.omit(l.gud$x)) # set NAs to mean until real traits are filled in
 
# get SLA per species
s.gud <- sla[sla$siteID=='Gudmedalen'|sla$siteID=='global',] # site subset
s.gud <- aggregate(sla$SLA_cm2_g, list(sla$species), FUN=mean) # species trait mean

M.s.gud <- abs(outer(s.gud$x, s.gud$x, "-"))
rownames(M.s.gud) <- Species$species -> colnames(M.s.gud)
M.s.gud[is.na(M.s.gud)] <- mean(na.omit(s.gud$x)) # set NAs to mean until real traits are filled in


# To do: save matrix objects to be loaded in S9 script

