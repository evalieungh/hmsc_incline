###############################################
#  explore trait values for specific species  #
###############################################

# script by EL
# started 2023-07-08

setwd(
  'C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/'
)

trait_df <- readRDS('data_processed/traits_localmax.Rds')

# focus on height and SLA.
# what is the mean, median, and standard deviation of trait values? 
mean(trait_df$spA_height_mm) # 168.1
median(trait_df$spA_height_mm) # 144.0
sd(trait_df$spA_height_mm) # 93.7

mean(trait_df$spA_SLA_cm2_g) # 246.8 
median(trait_df$spA_SLA_cm2_g) # 250.4
sd(trait_df$spA_SLA_cm2_g) # 100.5

# Let's make the simplistic assumption that species with high stature and SLA
# are better competitors. 
# what are the values for specific species? 
trait_df[trait_df$speciesA == "Pot_ere", # Potentilla erecta - average
         "spA_height_mm"] # 148 mm (about average)
trait_df[trait_df$speciesA == "Pot_ere",
         "spA_SLA_cm2_g"] # 258 (about average)

trait_df[trait_df$speciesA == "Vac_uli", # Vaccinium uliginosum - strong competitor?
         "spA_height_mm"] # 474 (tall)
trait_df[trait_df$speciesA == "Vac_uli",
         "spA_SLA_cm2_g"] # 130 (conservative)

trait_df[trait_df$speciesA == "Nar_str", # Nardus stricta - average
         "spA_height_mm"] # 154-270 (about average)
trait_df[trait_df$speciesA == "Nar_str",
         "spA_SLA_cm2_g"] # 118 (conservative)

trait_df[trait_df$speciesA == "Bis_viv", # Bistorta vivipara - average
         "spA_height_mm"] # 102-204 (about average)
trait_df[trait_df$speciesA == "Bis_viv",
         "spA_SLA_cm2_g"] # 200-256 (about average)

trait_df[trait_df$speciesA == "Vio_bif", # Viola biflora - average
         "spA_height_mm"] # 45-280 (generally shorter than 280)
trait_df[trait_df$speciesA == "Vio_bif",
         "spA_SLA_cm2_g"] # 446-476 (aquisitive)

trait_df[trait_df$speciesA == "Car_big", # Carex bigelowii - average
         "spA_height_mm"] # 105-179 (about average)
trait_df[trait_df$speciesA == "Car_big",
         "spA_SLA_cm2_g"] # 182-220 (slightly conservative)

trait_df[trait_df$speciesA == "Fes_rub", # Festuca rubra
         "spA_height_mm"] # 126-221 (about average)
trait_df[trait_df$speciesA == "Fes_rub",
         "spA_SLA_cm2_g"] # 114-127 ()

trait_df[trait_df$speciesA == "Fes_ovi", # Festuca ovina
         "spA_height_mm"] # 159-232
trait_df[trait_df$speciesA == "Fes_ovi",
         "spA_SLA_cm2_g"] # 168-632 

trait_df[trait_df$speciesA == "Sal_her", # Salix herbacea - poor competitor
         "spA_height_mm"] # 53-135 (short)
trait_df[trait_df$speciesA == "Sal_her",
         "spA_SLA_cm2_g"] # 173-243 (relatively conservative)

trait_df[trait_df$speciesA == "Des_ces", # Deschampsia cespitosa - strong competitor?
         "spA_height_mm"] # 309-366 (tall)
trait_df[trait_df$speciesA == "Des_ces",
         "spA_SLA_cm2_g"] # 161-211

trait_df[trait_df$speciesA == "Rum_ace", # Rumex acetosa - strong competitor?
         "spA_height_mm"] # 369 (tall)
trait_df[trait_df$speciesA == "Rum_ace",
         "spA_SLA_cm2_g"] # 348 (aquisitive)

trait_df[trait_df$speciesA == "", # 
         "spA_height_mm"] # 
trait_df[trait_df$speciesA == "",
         "spA_SLA_cm2_g"] # 

trait_df[trait_df$speciesA == "", # 
         "spA_height_mm"] # 
trait_df[trait_df$speciesA == "",
         "spA_SLA_cm2_g"] # 




