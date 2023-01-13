###################################
#   Read in data for HMSC model   #
###################################

# Script modified from HMSC course 2020 to run with my data.

# Need to provide an SXY file: study design (S) and/or covariates (X) and species data (Y) 
# The files TP and P are optional, so indicate with TRUE/FALSE if they are included or not
is.TP = FALSE # is there trait data? TO BE ADDED
is.P = FALSE # is there phylogeny data? TO BE ADDED

# READING IN SXY: 
SXY = read.csv("Data/SXY.csv", stringsAsFactors=TRUE)
SXY[1:5,1:10]
#split the SXY file into components
S=SXY[,1:4] # study design; Columns Site, Block, plot, subPlot
X=SXY[,5] # covariates to be used as predictors; Column prec (annual precipitation at site)
Y=SXY[,6:ncol(SXY)] # species data; presence-absence in subplots (could alternatively use subplot frequency, max 29 of 35 subplots per plot)
# What is not always easy is to decide what goes to S and what to X.
# As a general rule, include in S those variables that you think should be modelled as random effect,
# and in X those that you think should be modelled as fixed effects.

# Check that the data looks as it should!
{
View(S)
View(X)
View(Y)
}

# check that community data are numeric and have finite numbers. If the script
# writes "Y looks OK", you are ok.
if (is.numeric(as.matrix(Y)) || is.logical(as.matrix(Y)) && is.finite(sum(Y, na.rm=TRUE))) {
    print("Y looks OK")
} else {
	print("Y should be numeric and have finite values")	}
# Check that the study design data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(S))) {
  print("S has NA values - not allowed for")
} else {
  print("S looks ok")	}
# Check that the covariate data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(X))) {
  print("X has NA values - not allowed for")
} else {
  print("X looks ok")	}


# READING IN TP: traits (T) and/or phylogenetic information in table format (P)
if(is.TP){
  # Read in the species names as rownames, not as a column of the matrix
  TP = read.csv("Data/TP.csv", stringsAsFactors=TRUE,row.names = 1)
  # The script below checks if the species names in TP are identical and in the same order as in Y
  # If the script prints "species names in TP and SXY match", you are ok.
  # If it says that they do not match, you need to modify the files so that they match 
  if(all(rownames(TP)==colnames(Y))) {
    print("species names in TP and SXY match")
  } else{
    print("species names in TP and SXY do not match")
  }
  # Modify the next two lines to split your TP file to components that relate to
  # Tr: species traits (note that T is a reserved word in R and that's why we use Tr)
  # P: phylogenetic information given by taxonomical levels, e.g. order, family, genus, species
  # If you don't have trait data, indicate this by Tr=NULL. 
  # If TP does not have phylogenetic data (because you don't have such data at all, or because
  # it is given in tree-format, like is the case in this example), indicate this with P=NULL 
  Tr = TP[,1:2]
  P = NULL
  # Check that the data looks as it should!
  View(Tr)
  View(P)
  # Check that the Tr data do not have missing values (they are allowed for Y but not S, X, P or Tr)
  if (any(is.na(Tr))) {
    print("Tr has NA values - not allowed for")
  } else {
    print("Tr looks ok")	}
  # Check that the phylogenetic/taxonomic data do not have missing values (they are allowed for Y but not S, X, P or Tr)
  if (any(is.na(P))) {
    print("P has NA values - not allowed for")
  } else {
    print("P looks ok")	}
}

# READING IN P: phylogenetic information in tree format (P)
# we use ape package for trees, and P.tre must be in a format that ape understands
if(is.P){
  # Read in the phylogenetic tree using read.tree from ape
  library(ape)
  P = read.tree("Data/P.tre")
  # When you look at P (e.g. write P and press enter),
  # you should see that it is a phylogenetic tree which
  # is rooted and includes branch lengths and tip labels
  # The script below checks if the species names in P are identical (but not necessarily in the same order) as in Y
  # If the script prints "species names in P and SXY match", you are ok.
  # If it says that they do not match, you need to modify the files so that they match 
  if(all(sort(P$tip.label) == sort(colnames(Y)))){
    print("species names in P and SXY match")
  } else{
    print("species names in P and SXY do not match")
  }
  # Check that the data looks as it should!
  plot(P, cex=0.5)
}

save.image(file = 'Data/allData.R')
