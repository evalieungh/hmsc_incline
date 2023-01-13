#########################################
#    compare associations and traits    #
#########################################

# Omegas stored from Hmsc pipeline as .xlsx with associations, posterior probabilities of omega>0 and <0 (sum to 1). Manually save the omegas and positive probabilities as .csv files. 

# TO DO: clean first part of script to use data saved from S9_format_omegas instead!

# read data
#-------------------------------------
# association estimates (Hmsc omegas) and posterior probability
a_skj <- read.csv('Data/model_output/Omega_Skjellingahaugen_subplot.csv', row.names = 1)
a_gud <- read.csv('Data/model_output/Omega_Gudmedalen_subplot.csv',row.names = 1) # estimated associations
#pppr_gud <- read.csv('Data/model_output/pppr_Gudmedalen_subplot.csv') # positive association posterior probability
#plot(pppr_gud[,2],a_gud[,2]) # association estimates and posterior probabilities correlate strongly
a_lav <- read.csv('Data/model_output/Omega_Lavisdalen_subplot.csv',row.names = 1)
a_ulv <- read.csv('Data/model_output/Omega_Ulvehaugen_subplot.csv',row.names = 1)

# format data
#---------------------------------------
# set upper triangle of matrix to NA
a_gud[upper.tri(as.matrix(a_gud), diag=FALSE)] <- NA
pppr_gud[upper.tri(as.matrix(pppr_gud), diag=FALSE)] <- NA
# to do: make loop to repeat for other sites
M.h.gud[upper.tri(as.matrix(M.h.gud), diag=FALSE)] <- NA 
M.h.gud <- as.data.frame(M.h.gud)



# compare traits and associations
#--------------------------------------

# plots
plot(as.matrix(a_gud),as.matrix(M.h.gud),ylab = 'species-species trait difference',xlab = 'hmsc species-species association',main = 'height') # height
plot(as.matrix(a_gud),as.matrix(M.l.gud),ylab = 'species-species trait difference',xlab = 'hmsc species-species association',main = 'leaf area') # leaf area (cm2)
plot(as.matrix(a_gud),as.matrix(M.s.gud),ylab = 'species-species trait difference',xlab = 'hmsc species-species association',main = 'SLA') # specific leaf area (cm2/g)


# mantel test - https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-a-mantel-test-in-r/
library(ade4)
M1 <- dist(as.matrix(a_gud))
M2 <- dist(as.matrix(M.h.gud))
mantel.rtest(M1, M2, nrepet = 9999)
# Observation: -0.01784062 
# Based on 9999 replicates
# Simulated p-value: 0.7494
