#######
# selected species
######

# script by EL

# load data saved in S9_format_omegas
Mfull <- readRDS('data/a_obj_full.RData')


### COPIED IN FROM s10, NEED TO UPDATE OBJECT NAMES ###
# Closer look at well-studied species
#--------------------------------------------
# Look more closely at species with a lot of data from VCG and from other papers

# Viola biflora
hist(t(a_gud['Vio_bif',]))
sum(a_obj_full[[4]]['Vio_bif',])

# Viola palustris
(t(a_gud['Vio_pal',]))
hist(t(a_gud['Vio_pal',]))
sum(a_obj_full[[4]]['Vio_pal',])

# Veronica alpina
(t(a_gud['Ver_alp',]))
sum(a_obj_full[[4]]['Ver_alp',]) # looks like association optimum

# Sibbaldia procumbens
(t(a_gud['Sib_pro',]))
sum(a_obj_full[[4]]['Sib_pro',])


# Veronica alpina
# -------------------------
# sort association/co-occurrence matrix on species
