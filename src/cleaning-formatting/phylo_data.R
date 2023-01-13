###########################
#   Phylogeny data prep   #
###########################

# Script by Eva Lieungh

# NEW PLAN make a simple taxonomy-based tree to use in analysis of hmsc model *output*

library(ape)
library(tidyverse)

# Format species taxonomic data to the correct format
#----------------------------------------------------
# Correct: species,family,genus,epithet
# read in raw data file:
species <- read.csv('Data/specieslist.csv') # made in S0 script
names(species)[1] <- 'Species' # rename column to match tax
head(species)

# #Add epithet column by extracting it from the species name
# #NB! Two typos in the raw file fixed manually: Lines 55,86 had additional spaces. They have been removed manually.
# tax <- tax %>% 
#   separate(Full_name,into=c('genusagain','epithet'),sep = '\\s',remove = FALSE)
# 
# # Reorder and rename columns:
# tax <- tax %>%
#   select(species = Full_name,
#          order = Order,         # can't hurt to add this level too?
#          family = Family,
#          genus = genusagain,
#          epithet = epithet
#          )
# head(tax)
# 
# # Export
# write.csv(tax,'Data/taxonomy.csv')


# prep species list to get phylogenetic tree
#-------------------------------------------------------------------
# input format to TimeTree must be Genus species on separate rows in a text file
species <- merge(species,tax, # merge the two data sets by shortname
                 by = 'Species',
                 all.x = TRUE) # keep all rows from 'species' but not unmatched from 'systematics'
# add taxonomy info for species that miss it
which(is.na(species$Full_name)) # which rows have missing taxonomy data?


species <- species %>%  # rename columns and drop unneccessary ones
  select(shortname = Species,
         longname = Full_name)
             
## replace genus level records with a common/representative species
species[] <- lapply(species, as.character) # change from factor to character
species <- mutate_if(species, is.character, 
                      str_replace_all, 
                      pattern = c('Alchemilla sp.','Euphrasia sp.','Taraxacum sp.'), 
                      replacement = c('Alchemilla vulgaris','Euphrasia wettsteinii','Taraxacum croceum')) # check that this worked for all the species...

### for some reason Tofieldia pusilla is present in this data set but not in the others. Removing it for now.
species <- species[!grepl('Tofieldia', species$longname),]

## write the species list as a text file with each species longname on a separate row
length(na.omit(unique(species$longname))) # 66 species
write.table(na.omit(unique(species$longname)),'Data/specieslistlongname.txt',
            row.names = FALSE, col.names = FALSE, quote = FALSE)
## use the .txt file to create a phylogeny on timetree.org, save as Newick file
## Timetree.org gives "15 unresolved names": 
# Carex bigelowii
# Carex leporina
# Carex norvegica
# Empetrum nigrum
# Epilobium anagallidifolium
# Geranium sylvaticum
# Phleum alpinum
# Poa alpina
# Potentilla crantzii
# Salix herbacea
# Solidago virgaurea
# Taraxacum sp.
# Thalictrum alpinum
# Veronica alpina
# Viola biflora


# load and handle phylogenetic tree
#-------------------------------------------------------------------
## double check that all, and the correct, species are included! 
## (22.05.2020 Hmsc() still gives subscript out of bounds error - probably because of long vs short version of species names)
mytree <- read.tree('Data/specieslistlongname.nwk')
class(mytree) # phylo class
plot.phylo(mytree, # phylogeny plot - check if it looks sensible
           cex = 0.5)
checkValidPhylo(mytree) # Check the Structure of a "phylo" Object
summary.phylo(mytree) # Summary of a Phylogeny - 51 tips (species)

# change tip labels (=species names) from long names & representative species back to short names
mytree$tip.label[mytree$tip.label=='Alchemilla_vulgaris'] <- 'Alchemilla_sp.'
mytree$tip.label[mytree$tip.label=='Euphrasia_wettsteinii'] <- 'Euphrasia_sp.'
mytree$tip.label[mytree$tip.label=='Taraxacum_croceum'] <- 'Taraxacum_sp.'

# some species have changed names inside the TimeTree procedure (synonyms etc) and need to be changed back
mytree$tip.label[mytree$tip.label=='Avenella_flexuosa'] <- 'Deschampsia_flexuosa'
mytree$tip.label[mytree$tip.label=='Bistorta_vivipara'] <- 'Persicaria_vivipara'
mytree$tip.label[mytree$tip.label=='Scorzoneroides_autumnalis'] <- 'Leontodon_autumnalis'
mytree$tip.label[mytree$tip.label=='Pilosella_officinarum'] <- 'Hieracium_pilosella'
mytree$tip.label[mytree$tip.label=='Gnaphalium_supinum'] <- 'Omalotheca_supina'
### NB! check for more cases like this when using the full data set

# match the species names with the short names and replace them in the phylo object
speciesnames <- data.frame(tip.label = as.character(testsubset$Species),shortname = as.character(testsubset$shortname), stringsAsFactors = FALSE) # 
speciesnames$tip.label <- sub(" ", "_", speciesnames$tip.label) # change from space to underscore format so the names are identical
head(cbind(mytree$tip.label,speciesnames))
## new species names vector must have the same order as the original one
tip.label <- data.frame(tip.label = as.character(mytree$tip.label), stringsAsFactors = FALSE)
head(cbind(tip.label,speciesnames))
orderedSpeciesList <- left_join(tip.label,speciesnames, by = 'tip.label')

# some of the species are missing shortnames. Add them:
orderedSpeciesList$shortname[orderedSpeciesList$tip.label=='Alchemilla_sp.'] <- 'Alc_sp'
orderedSpeciesList$shortname[orderedSpeciesList$tip.label=='Euphrasia_sp.'] <- 'Eup_sp'
orderedSpeciesList$shortname[orderedSpeciesList$tip.label==''] <- ''

identical(as.character(mytree$tip.label),as.character(orderedSpeciesList$tip.label)) # check if the species are the same and in the same order
mytree$tip.label <- orderedSpeciesList$shortname

# save the phylogeny (to be picked up in HMSC script)
write.tree(mytree,'Data/phylogeny.nwk') # write tree in nwk format

# clean up the work environment a bit
rm(mytree, orderedSpeciesList, speciesnames, tip.label)




#-------------------------------------------------
## some potentially useful APE functions:
# drop.tip() # Remove Tips in a Phylogenetic Tree
# collapse.singles() # Collapse Single Nodes
# as.phylo.formula() # Conversion from Taxonomy Variables to Phylogenetic Trees
# checkValidPhylo() # Check the Structure of a "phylo" Object
# plotTreeTime() # Plot Tree With Time Axis
# zoom() # Zoom on a Portion of a Phylogeny
# weight.taxo() # Define Similarity Matrix
# vcv() # Phylogenetic Variance-covariance or Correlation Matrix


# in case of splitting species into stages, add them as 'species' to their genus with a 
# non-zero distance that is still smaller than between-species distances.

