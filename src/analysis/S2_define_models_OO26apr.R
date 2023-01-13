setwd("U:/all stuff/manuscripts/InPreparation/Eva Lieungh alpine plants")
library(Hmsc)

da = read.csv("data/SXY.csv")
n = dim(da)[1]
sub.plot.number = rep(NA,n)
for(i in 1:n){
  tmp = da$subPlotID[i]
  tmp1 = unlist(gregexpr('_', tmp))
  sub.plot.number[i] = as.numeric(substr(tmp,start=tmp1[length(tmp1)]+1,stop=nchar(tmp)))
}
loc.x = rep(c(1,2,3,4,5,6,7),5)[sub.plot.number]
loc.y = c(rep(5,7),rep(4,7),rep(3,7),rep(2,7),rep(1,7))[sub.plot.number]

XData = data.frame(prec=as.numeric(da$prec))
Y = as.matrix(da[,-(1:5)])
studyDesign = data.frame(site=as.factor(da$Site),
                         block=as.factor(da$blockID),
                         plot=as.factor(da$plotID),
                         subplot=as.factor(da$subPlotID))

xy = matrix(NA,ncol = 2,nrow = n)
xy[,1] = 100*as.numeric(studyDesign$plot)+loc.x
xy[,2] = loc.y
colnames(xy) = c("x","y")
rownames(xy) = studyDesign$subplot
head(xy)
plot(xy[,1],xy[,2],xlim = c(0,300))

rL.site = HmscRandomLevel(units = levels(studyDesign$site))
rL.block = HmscRandomLevel(units = levels(studyDesign$block))
rL.plot = HmscRandomLevel(units = levels(studyDesign$plot))
rL.subplot = HmscRandomLevel(units = levels(studyDesign$subplot))
#rL.subplot = HmscRandomLevel(sData = xy,sMethod = "NNGP",nNeighbours = 28)

m = Hmsc(Y=Y,
         XFormula = ~1,XData = XData,
         distr = "probit",
         studyDesign = studyDesign,
         ranLevels = list(site=rL.site,
                          block=rL.block,
                          plot=rL.plot,
                          subplot = rL.subplot)
)
models = list(m)
modelnames = c("global")

sites = levels(studyDesign$site)
for(i in 1:length(sites)){
  sel = which(studyDesign$site==(sites[i]))
  studyDesign.local = droplevels(studyDesign[sel,])
  rL.block = HmscRandomLevel(units = levels(studyDesign.local$block))
  rL.plot = HmscRandomLevel(units = levels(studyDesign.local$plot))
  rL.subplot = HmscRandomLevel(units = levels(studyDesign.local$subplot))
  m = Hmsc(Y=Y[sel,],
           XFormula = ~1,XData = as.data.frame(XData[sel,]),
           distr = "probit",
           studyDesign = studyDesign.local,
           ranLevels = list(block=rL.block,
                            plot=rL.plot,
                            subplot = rL.subplot)
  )                
  models[[i+1]] = m
  modelnames = c(modelnames,sites[i])
}
for(i in 1:length(models)){
  print(modelnames[i])
  sampleMcmc(models[[i]],samples = 2)
}

save(models,modelnames,file = "models/unfitted_models")
