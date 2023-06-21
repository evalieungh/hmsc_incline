###################
#  Define models  #
###################

# script by Otso Ovaskainen, Eva Lieungh
# started 2022-04-26

setwd("C:/Users/evaler/OneDrive - Universitetet i Oslo/Eva/PHD/hmsc_incline/")
library(Hmsc)

da = read.csv("data/SXY.csv")
n = dim(da)[1]

# create representation of spatial study design of subplots
sub.plot.number = rep(NA,n)
for(i in 1:n){
  tmp = da$subPlotID[i]
  tmp1 = unlist(gregexpr('_', tmp))
  sub.plot.number[i] = as.numeric(substr(tmp,start=tmp1[length(tmp1)]+1,stop=nchar(tmp)))
}
loc.x = rep(c(1,2,3,4,5,6,7),5)[sub.plot.number]
loc.y = c(rep(5,7),rep(4,7),rep(3,7),rep(2,7),rep(1,7))[sub.plot.number]
# check that it looks OK
plot(loc.x,loc.y)
# NB: the units of spatial coordinates are subplots, 
# i.e. subplot 24 has coords x=3, y=2. To get actual distances, 
# multiply with 5 cm. Model is set up to consider neighbours 
# up to distances=6, so all subplots within same plot 
# but not between plots.

# read covariates (X)
XData = data.frame(precipitation=as.numeric(da$prec),
                   soilmoisture=as.numeric(da$soil_mst_mean),
                   soiltemp=as.numeric(da$soil_tmp_mean))
# ... species subplot occurrences (Y)
Y = as.matrix(da[,-(1:7)])
# and study design
studyDesign = data.frame(site=as.factor(da$site),
                         block=as.factor(da$blockID),
                         plot=as.factor(da$plotID),
                         subplot=as.factor(da$subPlotID))

xy = matrix(NA,ncol = 2,nrow = n)
xy[,1] = 100*as.numeric(studyDesign$plot)+loc.x
xy[,2] = loc.y
colnames(xy) = c("x","y")
rownames(xy) = studyDesign$subplot
head(xy)
# plot to check it looks OK
plot(xy[,1],xy[,2],
     xlim = c(50,300))

# set study design as random levels
rL.site = HmscRandomLevel(units = levels(studyDesign$site))
rL.block = HmscRandomLevel(units = levels(studyDesign$block))
rL.plot = HmscRandomLevel(units = levels(studyDesign$plot))
rL.subplot = HmscRandomLevel(units = levels(studyDesign$subplot))
#rL.subplot = HmscRandomLevel(sData = xy,sMethod = "NNGP",nNeighbours = 28)

# define global model
m = Hmsc(Y=Y,
         XFormula = ~ 1 + soiltemp + soilmoisture,
         XData = XData,
         distr = "probit",
         studyDesign = studyDesign,
         ranLevels = list(
           site=rL.site,
           block=rL.block,
           plot=rL.plot,
           subplot = rL.subplot)
)
models = list(m)
modelnames = c("global")

# define site-specific models
sites = levels(studyDesign$site)
for(i in 1:length(sites)){
  sel = which(studyDesign$site == (sites[i]))
  studyDesign.local = droplevels(studyDesign[sel,])
  rL.block = HmscRandomLevel(units = levels(studyDesign.local$block))
  rL.plot = HmscRandomLevel(units = levels(studyDesign.local$plot))
  rL.subplot = HmscRandomLevel(units = levels(studyDesign.local$subplot))
  m = Hmsc(Y = Y[sel,],
           XFormula = ~ 1 + soiltemp + soilmoisture,
           XData = as.data.frame(XData[sel,]),
           distr = "probit",
           studyDesign = studyDesign.local,
           ranLevels = list(
             block = rL.block,
             plot = rL.plot,
             subplot = rL.subplot)
  )                
  models[[i+1]] = m
  modelnames = c(modelnames,sites[i])
}

for(i in 1:length(models)){
  print(modelnames[i])
  sampleMcmc(models[[i]],samples = 2)
}

save(models,modelnames,file = "data_processed/models/unfitted_models")
