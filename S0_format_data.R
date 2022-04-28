####################################
# Data formatting for HMSC scripts #
####################################

# Need to make an SXY file: study design (S) and/or covariates (X) and species data (Y) 
# To include traits (Tr) and phylogeny (P), files TP and P are also needed. 

library(tidyverse)

# SXY
#------------------------------------------
SXY <- read.csv('Data/Community/INCLINE_community_2018_clean.csv') # presence-absence S (study design) and Y (species) data from 2018
SXY[1:3,1:10]

# make precipitation covariate
SXY <- SXY %>%
  mutate(prec = as.integer(ifelse(Site=='Skjellingahaugen',2725,
                       ifelse(Site=='Gudmedalen',1925,
                              ifelse(Site=='Lavisdalen',1321,
                                     ifelse(Site=='Ulvehaugen',593,(.)))))))

SXY <- SXY %>%
  select(Site:subPlotID,prec,Ach_mil:Vio_sp)
SXY[1:5,1:10]

# remove too rare or common species
Y = SXY[,6:ncol(SXY)] # extract species data
ubiquitous = round(nrow(Y) * 0.9) # define threshold for being too common
rare = round(nrow(Y) * 0.01) # define threshold for being too rare
remove_pa = which(colSums(Y>0)<rare|colSums(Y>0)>ubiquitous)
length(remove_pa) # how many species are too rare, or ubiquitous?
Y <- Y[,-remove_pa] # remove those species

colnames(Y)[grepl('_cf',colnames(Y))] # check for uncertain species
colnames(Y)[grepl('_sp',colnames(Y))] # check for genus-level records. Include of exclude?

species <- c(names(Y)) # final list of species to include (pick up to make phylo tree and traits df)
SXY <- data.frame(SXY[,1:5],Y) # save SXY with the correct species

# export SXY file and species list
write.csv(SXY,'Data/SXY.csv',row.names = FALSE)
write.csv(species,'Data/specieslist.csv',row.names = FALSE)

# Trait data
#------------------------------------------
Tr <- read.csv('Data/TP.csv')
head(Tr)

# get traits for all species in Vestland Climate grid (?) and extract some traits for the species list

# Phylo data
#------------------------------------------
# To do: open Data/specieslist_for_TimeTree.csv and change to full species names, 
# get tree from TimeTree.org
# change tip names so they match Y in SXY

library(ape)
P <- read.tree('Data/P.tre') # tree format
P




# Validation data from FunCab project?
#-----------------------------------------------
# they have subplot-level PA data for 5 subplots (not adjacent?) https://github.com/Between-the-Fjords/funcab_data/issues/44 


