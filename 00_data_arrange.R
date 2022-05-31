folder0<-getwd()

setwd (paste(folder0,"/DATA",sep=""))

# Load packages
library(Morpho)
library(geomorph)
library(abind)
library(RRPP)
library(ggpubr)
library(standardize)
library(lmerTest)
library(vioplot)
library(randomcoloR)
library(ggplot2)
library(ppcor)
library(ggfortify)
library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)
library(corrplot)
library(randomcoloR)
library(mapplots)


# Load data
slids<-read.table("slidersR.txt")
links<-read.table("linksR.txt")
#All data except for Asia
symdat<-read.delim("symdat.txt",stringsAsFactors = F)

#Asian samples
vdat<-read.delim("VN_hanoi_data.txt",stringsAsFactors = F)
idat<-read.delim("India_data.txt",stringsAsFactors = F)

names(symdat)
names(vdat)
names(idat)
idat$Ethnicity

symdat$id_tps
symdat$name

vdat$id_tps
idat$ID

#Rearrange to fit the original data format
vdat<-data.frame(id_tps=vdat[,1],name=vdat[,2],sex=vdat[,3],set=vdat[,4],nat=rep("VIE",nrow(vdat)),
                 age=vdat[,6],weight=vdat[,7],height=vdat[,8],M_attractiveness=vdat[,9])

#Remove two targets with tilted head and missing attractiveness
vdat<-vdat[-match(c(2080,1031,2031),vdat$name),]

idat<-data.frame(id_tps=idat[,1],name=idat[,1],sex=idat[,3],set=rep(NA,nrow(idat)),nat=rep("IND",nrow(idat)),
                 age=idat[,4],weight=rep(NA,nrow(idat)),height=rep(NA,nrow(idat)),M_attractiveness=idat[,5])


names(symdat)
names(vdat)
names(idat)

symdat<-rbind(symdat,vdat,idat)


#Order the dataset according the sex within each nation
orig.ord<-unique(symdat$nat)
symdat<-symdat[with(symdat, order(sex)),]
symdat<-symdat[order(match(symdat$nat,orig.ord)*100000+1:nrow(symdat)),]
unique(symdat$nat)

# Rename Attractiveness scale
names(symdat)[names(symdat) == "M_attractiveness"] <- "Attractiveness"
#Check the ordering
symdat$sex
symdat$nat

# Read .tps files with face coordinates for each country
brazf <- readland.tps ("F_braz.tps")
brazm <- readland.tps ("M_braz.tps")
braz <- abind (brazf, brazm)

fcmr12 <- readland.tps ("F_cmr12.tps")
mcmr12 <- readland.tps ("M_cmr12.tps")
fcmr13 <- readland.tps ("F_cmr13.tps")
mcmr13 <- readland.tps ("M_cmr13.tps")
fcmr16 <- readland.tps ("F_cmr16.tps")
mcmr16 <- readland.tps ("M_cmr16.tps")

cmrf <- abind (fcmr12, fcmr13, fcmr16)
cmrm <- abind (mcmr12, mcmr13, mcmr16)
cmr <- abind (cmrf, cmrm)

colf <- readland.tps ("F_col.tps")
colm <- readland.tps ("M_col.tps")
col <- abind (colf, colm)

cz120f <- readland.tps ("F_cz120.tps")
cz120m <- readland.tps ("M_cz120.tps")
czf <- readland.tps ("F_cz16.tps")
czm <- readland.tps ("M_cz16.tps")
cz <- abind (cz120f, czf, cz120m, czm)

namf <- readland.tps ("F_nam.tps")
namm <- readland.tps ("M_nam.tps")
nam <- abind (namf, namm)

rof <- readland.tps ("F_ro.tps")
rom <- readland.tps ("M_ro.tps")
ro <- abind (rof, rom)

trf <- readland.tps ("F_tur.tps")
trm <- readland.tps ("M_tur.tps")
tr <- abind (trf, trm)

ukf <- readland.tps ("F_uk.tps")
ukm <- readland.tps ("M_uk.tps")
uk <- abind (ukf, ukm)

#We again remove the two tilted targets from the vietnameese datasets
vif <- readland.tps ("VNM_females.tps",specID = "imageID")
vif<-vif[,,-match("2080",dimnames(vif)[[3]])]
str(vif)
vim <- readland.tps ("VNM_males.tps",specID = "imageID")
vim<-vim[,,-match(c("1031","2031"),dimnames(vim)[[3]])]
str(vim)
vi <- abind (vif, vim)

iif <- readland.tps ("India_F.tps")
iim <- readland.tps ("India_M.tps")
ii <- abind (iif, iim)

# Combine all the data
one <- abind (cmr, cz, tr, nam, ro, uk, col, braz, vi, ii)

#Sample sizes
(N<-tapply(symdat$weight,symdat$nat,length))
#missing weights
(Nwmiss<-tapply(symdat$weight,symdat$nat,function(x){sum(is.na(x))}))
#Sample sizes usable in the analysis with weight and BMI
N-Nwmiss

#If you want to remove the rows where the information abot body height/weight is missing, just uncomment two following rows
#one <- one[,,!is.na(symdat$height)]
#symdat<-symdat[!is.na(symdat$height),]

# Divide the data to two sets according to the sex
fdat <- subset(symdat, sex=="F")
mdat <- subset(symdat, sex=="M")

# Connect these data frames to the final data 
dat<- rbind(fdat, mdat)

# Procrustes analysis of the full dataset
gpone<-gpagen(one,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gpone)

# Symmetrize shape coordinates for some later calculations
left <- c(50,51,52,53,54,55,56,57,58,5,7,12,10,70,15,17,18,20,26,23,21,22,21,25,24,71,35,46,45,44,34,49,48,47)
right<-c(59,60,61,62,63,64,65,66,67,4,6,11,9,69,14,16,19,27,33,29,28,30,28,32,31,72,37,38,39,40,36,41,42,43)
pairedLM<-cbind(left, right)

str(one)
nrow(symdat)
summary(as.factor(symdat$nat))
str(braz)
str(cmr)
str(col)
str(cz)
str(ii)
str(nam)
str(ro)
str(tr)
str(uk)
str(vi)


sgpone <- symmetrize (gpone$coords, pairedLM)
plot(gpagen(sgpone, PrinAxes = F))

# Create geomorph data frames for non- and - symetrized data with varibales corresponding to several variables from symdat
onegdf <- geomorph.data.frame(gpone, sex = symdat$sex, set = symdat$set, nat = symdat$nat, age = symdat$age, weight = symdat$weight, height = symdat$height, attractiveness = symdat$Attractiveness)

sonegdf<- geomorph.data.frame(coords = sgpone, sex = symdat$sex, set = symdat$set, nat = symdat$nat, age = symdat$age, weight = symdat$weight, height = symdat$height, attractiveness = symdat$Attractiveness)
  
# Subset landmark coordinates according to sex 
shapesex<- coords.subset(onegdf$coords, onegdf$sex)
symshapesex <- coords.subset (sonegdf$coords, sonegdf$sex)

# Create array ordered by sex from the shapesex data frame
shape <- abind (shapesex$F, shapesex$M)
symshape <- abind (symshapesex$F, symshapesex$M)

# Generalized procrustes analysis of the resulting array
gpsh<-gpagen(shape,  ProcD = F, curves = NULL, PrinAxes = F, Proj = TRUE)
plot(gpsh)

sgpsh<-gpagen(symshape,  ProcD = F, curves = NULL, PrinAxes = F, Proj = TRUE)
plot(sgpsh)

# Create geomorph data frame with varibales corresponding to several vectors from dat which has the same order of lines as the set of shapes in gpsh - females then males
gtf <- geomorph.data.frame(gpsh, sex = dat$sex, set =  dat$set, nat = dat$nat, age = dat$age, weight = dat$weight, height = dat$height, attractiveness = dat$Attractiveness)

sgtf <- geomorph.data.frame(sgpsh, sex = dat$sex, set =  dat$set, nat = dat$nat, age = dat$age, weight = dat$weight, height = dat$height, attractiveness = dat$Attractiveness)

# Investigate the outliers
invisible(capture.output(
  plotOutliers(gtf$coords, groups = gtf$sex, inspect.outliers = FALSE)
))
# Some points are indicated as potential outliers, but all the data points create a smooth curve, so we decide to retain the whole data

plotTangentSpace(gtf$coords, axis1=1, axis2=2, warpgrids=F, groups = gtf$sex, legend = T)
plotTangentSpace(gtf$coords, axis1=2, axis2=3, warpgrids=F, groups = gtf$sex, legend = T)
plotTangentSpace(gtf$coords, axis1=1, axis2=2, warpgrids=F, groups = gtf$nat, legend = T)
plotTangentSpace(gtf$coords, axis1=2, axis2=3, warpgrids=F, groups = gtf$nat, legend = T)
# Scatter plots do not indicate any problems of extreme outliers

# **************************************************************




# A-SYMMETRY


# calculate asymmetry scores for whole dataset ...by multiplication of x-coordinates of all landmarks by +-1), re-labelling of the bilaterally paired landmarks and averaging the original and mirrored configurations


plot(gpagen(gpsh$coords, PrinAxes = F))
X<-as.data.frame(two.d.array(gpsh$coords))
Xr <- X*matrix (rep(c(-1,1), each=nrow(X),times=ncol(X)/2), nrow = nrow(X), ncol = ncol (X))

Xr <- as.data.frame (Xr)
names(Xr)
coor<-Xr

# extract numbers from column names
cisla <- as.numeric(unlist(regmatches(names(coor), gregexpr("[[:digit:]]+", names(coor)))))
cisla2 <- cisla # copy

# re-label
left<-c(50,51,52,53,54,55,56,57,58,5,7,12,10,70,15,17,18,20,26,23,21,22,21,25,24,71,35,46,45,44,34,49,48,47)
right<-c(59,60,61,62,63,64,65,66,67,4,6,11,9,69,14,16,19,27,33,29,28,30,28,32,31,72,37,38,39,40,36,41,42,43)

# extract numbers from column names
cisla <- as.numeric(unlist(regmatches(names(coor), gregexpr("[[:digit:]]+", names(coor)))))
cisla2 <- cisla # copy

# exchange
pozice <- match(cisla,left)[-which(is.na(match(cisla,left)))]
pozice2 <- match(cisla,right)[-which(is.na(match(cisla,right)))]

cisla[-which(is.na(match(cisla,left)))] <- right[pozice]
cisla[-which(is.na(match(cisla2,right)))] <- left[pozice2]
cisla

# ordering
poradi <- order(cisla)

sloupec <- paste(cisla,c(".X",".Y"),sep="")
names(coor) <- sloupec
coor<-coor[,poradi]

# Plot original and reflected configurations

plot(gpagen(arrayspecs(coor, 72, 2), PrinAxes = F))
plot(gpagen(arrayspecs(X, 72, 2), PrinAxes = F))

# join and average original and reflected confidurations and plot the result
Xsym <- abind (arrayspecs(X, 72, 2), arrayspecs(coor, 72, 2))

plot(gpagen(Xsym, PrinAxes = F))

XSYM <-(Xsym [1:72, 1:2, 1:length(dat$nat)] + Xsym [1:72, 1:2, (1+length(dat$nat)):(length(dat$nat)*2)])/2

plot(gpagen(XSYM, PrinAxes = F))



# calculating the scores of asymmetry (= the Euclidean distance between original and mirrored configurations)

symscore <-rep (0,length(dat$nat))
for (i in 1:length(dat$nat))
{
  origos <- (X) [i,]
  reflect <- (coor) [i,]
  
  symscore [i]<-sqrt(sum((origos-reflect)^2))
}

hist(symscore)

# male & female symmetry scores altogether and for each population

fsymscore <- symscore [dat$sex == "F"]
msymscore <- symscore [dat$sex == "M"]

cmrFS <- fsymscore [fdat$nat == "CMR"]
cmrMS <- msymscore [mdat$nat == "CMR"]

brazFS <- fsymscore [fdat$nat == "BRAZ"]
brazMS <- msymscore [mdat$nat == "BRAZ"]

colFS <- fsymscore [fdat$nat == "COL"]
colMS <- msymscore [mdat$nat == "COL"]

czFS <- fsymscore [fdat$nat == "CZ"]
czMS <- msymscore [mdat$nat == "CZ"]

roFS <- fsymscore [fdat$nat == "RO"]
roMS <- msymscore [mdat$nat == "RO"]

trFS <- fsymscore [fdat$nat == "TR"]
trMS <- msymscore [mdat$nat == "TR"]

ukFS <- fsymscore [fdat$nat == "UK"]
ukMS <- msymscore [mdat$nat == "UK"]

namFS <- fsymscore [fdat$nat == "NAM"]
namMS <- msymscore [mdat$nat == "NAM"]

viFS <- fsymscore [fdat$nat == "VIE"]
viMS <- msymscore [mdat$nat == "VIE"]

iiFS <- fsymscore [fdat$nat == "IND"]
iiMS <- msymscore [mdat$nat == "IND"]

#concatenate the nation specific scores into vectors of all asymmetry scores 
fsymscores1 <- c(brazFS, cmrFS, colFS, czFS, iiFS, namFS, roFS, trFS, ukFS, viFS)
msymscores1 <- c(brazMS, cmrMS, colMS, czMS, iiMS, namMS, roMS, trMS, ukMS, viMS)
symscores<-c(fsymscores1,msymscores1)



#   AVERAGENESS

## Function for calculating distance from average

dist.f.mean <- function (x) {
  
  lokanf<-rep(0,dim (x) [3])
  for (i in 1:dim (x) [3])
    
  {
    puvfanf<-(x)[,,i]
    
    lokanf[i]<-(sum(rowSums((puvfanf-mshape(x))^2)))^0.5
  }
  
  lokanf}

## subset male and female coordinates by nationality 

Fshape<- coords.subset(shapesex$F, fdat$nat)
Mshape<- coords.subset(shapesex$M, mdat$nat)
sFshape<- coords.subset(symshapesex$F, fdat$nat)
sMshape<- coords.subset(symshapesex$M, mdat$nat)

## Caculate distance from average

### AVRG - NONSYMMMETRIZED coordinates (gtf$coords, shapesex)

globalmavrg <- dist.f.mean (shapesex$M)
globalfavrg <- dist.f.mean (shapesex$F)

fcmravrg <- dist.f.mean (Fshape$CMR)
mcmravrg <- dist.f.mean (Mshape$CMR)

fbrazavrg <- dist.f.mean (Fshape$BRAZ)
mbrazavrg <- dist.f.mean (Mshape$BRAZ)

fcolavrg <- dist.f.mean (Fshape$COL)
mcolavrg <- dist.f.mean (Mshape$COL)

fczavrg <- dist.f.mean (Fshape$CZ)
mczavrg <- dist.f.mean (Mshape$CZ)

fnamavrg <- dist.f.mean (Fshape$NAM)
mnamavrg <- dist.f.mean (Mshape$NAM)

ftravrg <- dist.f.mean (Fshape$TR)
mtravrg <- dist.f.mean (Mshape$TR)

froavrg <- dist.f.mean (Fshape$RO)
mroavrg <- dist.f.mean (Mshape$RO)

fukavrg <- dist.f.mean (Fshape$UK)
mukavrg <- dist.f.mean (Mshape$UK)

fviavrg <- dist.f.mean (Fshape$VIE)
mviavrg <- dist.f.mean (Mshape$VIE)

fiiavrg <- dist.f.mean (Fshape$IND)
miiavrg <- dist.f.mean (Mshape$IND)

### SAVRG - SYMMMETRIZED coordinates (sgtf$coords, symshapesex)

sglobmavrg <- dist.f.mean (symshapesex$M)
sglobfavrg <- dist.f.mean (symshapesex$F)


sfcmravrg <- dist.f.mean (sFshape$CMR)
smcmravrg <- dist.f.mean (sMshape$CMR)

sfbrazavrg <- dist.f.mean (sFshape$BRAZ)
smbrazavrg <- dist.f.mean (sMshape$BRAZ)

sfcolavrg <- dist.f.mean (sFshape$COL)
smcolavrg <- dist.f.mean (sMshape$COL)

sfczavrg <- dist.f.mean (sFshape$CZ)
smczavrg <- dist.f.mean (sMshape$CZ)

sfnamavrg <- dist.f.mean (sFshape$NAM)
smnamavrg <- dist.f.mean (sMshape$NAM)

sftravrg <- dist.f.mean (sFshape$TR)
smtravrg <- dist.f.mean (sMshape$TR)

sfroavrg <- dist.f.mean (sFshape$RO)
smroavrg <- dist.f.mean (sMshape$RO)

sfukavrg <- dist.f.mean (sFshape$UK)
smukavrg <- dist.f.mean (sMshape$UK)

sfviavrg <- dist.f.mean (sFshape$VIE)
smviavrg <- dist.f.mean (sMshape$VIE)

sfiiavrg <- dist.f.mean (sFshape$IND)
smiiavrg <- dist.f.mean (sMshape$IND)

##Concatenate population specific AVRG and SAVRG separately for males and females
favrg <- c(fbrazavrg, fcmravrg, fcolavrg, fczavrg, fiiavrg, fnamavrg, froavrg, ftravrg, fukavrg, fviavrg)
mavrg <- c(mbrazavrg, mcmravrg, mcolavrg, mczavrg, miiavrg, mnamavrg, mroavrg, mtravrg, mukavrg, mviavrg)
avrg <- c(favrg, mavrg)

sfavrg <- c(sfbrazavrg, sfcmravrg, sfcolavrg, sfczavrg, sfiiavrg, sfnamavrg, sfroavrg, sftravrg, sfukavrg, sfviavrg)
smavrg <- c(smbrazavrg, smcmravrg, smcolavrg, smczavrg, smiiavrg, smnamavrg, smroavrg, smtravrg, smukavrg, smviavrg)
savrg <- c(sfavrg, smavrg)




# SEXUAL SHAPE DIMORPHISM

## Analysis of variance in the face shape explained by sex
reg1<-procD.lm(coords ~ sex, iter = 99, data = gtf, weights = NULL)
summary(reg1)

## Extract the centers of the sex-specific clouds of points in the multidemensional space. Intercept corresponds to the female mean, sexM slope to the difference between male and female mean.
coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

## Projection of facial shapes on the vector connecting female and male mean, the resulting score is saved as sexscores variable
scores <- two.d.array (gpsh$coords) %*% t(coefficients)
sexscores <- scores [,2]
hist(sexscores)

## Save these sexscores for a future reference to an object with "original" sexscores
orig_sexscores<-sexscores

## Restart graphics window
dev.off()


## Calcucate Sexual Shape Dimoprhism scores separately for each population
onecoords<-coords.subset(onegdf$coords, onegdf$nat)

### Brazil
brazgdf <- geomorph.data.frame(coords = onecoords$BRAZ, sex = dat$sex[dat$nat=="BRAZ"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = brazgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$BRAZ) %*% t(coefficients)
brazsc <- scores [,2]

fbrazsc <- brazsc [which(dat$sex[dat$nat=="BRAZ"]=="F")]
mbrazsc <- brazsc [which(dat$sex[dat$nat=="BRAZ"]=="M")]

brazcoef<-t(coefficients)[,2]

### cmr
cmrgdf <- geomorph.data.frame(coords = onecoords$CMR, sex = dat$sex[dat$nat=="CMR"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = cmrgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$CMR) %*% t(coefficients)
cmrsc <- scores [,2]

fcmrsc <- cmrsc [which(dat$sex[dat$nat=="CMR"]=="F")]
mcmrsc <- cmrsc [which(dat$sex[dat$nat=="CMR"]=="M")]

cmrcoef<-t(coefficients)[,2]

### col
colgdf <- geomorph.data.frame(coords = onecoords$COL, sex = dat$sex[dat$nat=="COL"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = colgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$COL) %*% t(coefficients)
colsc <- scores [,2]

fcolsc <- colsc [which(dat$sex[dat$nat=="COL"]=="F")]
mcolsc <- colsc [which(dat$sex[dat$nat=="COL"]=="M")]

colcoef<-t(coefficients)[,2]

### cz
czgdf <- geomorph.data.frame(coords = onecoords$CZ, sex = dat$sex[dat$nat=="CZ"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = czgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$CZ) %*% t(coefficients)
czsc <- scores [,2]

fczsc <- czsc [which(dat$sex[dat$nat=="CZ"]=="F")]
mczsc <- czsc [which(dat$sex[dat$nat=="CZ"]=="M")]

czcoef<-t(coefficients)[,2]

### nam
namgdf <- geomorph.data.frame(coords = onecoords$NAM, sex = dat$sex[dat$nat=="NAM"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = namgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$NAM) %*% t(coefficients)
namsc <- scores [,2]

fnamsc <- namsc [which(dat$sex[dat$nat=="NAM"]=="F")]
mnamsc <- namsc [which(dat$sex[dat$nat=="NAM"]=="M")]

namcoef<-t(coefficients)[,2]

### ro
rogdf <- geomorph.data.frame(coords = onecoords$RO, sex = dat$sex[dat$nat=="RO"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = rogdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$RO) %*% t(coefficients)
rosc <- scores [,2]

frosc <- rosc [which(dat$sex[dat$nat=="RO"]=="F")]
mrosc <- rosc [which(dat$sex[dat$nat=="RO"]=="M")]

rocoef<-t(coefficients)[,2]

### tr
trgdf <- geomorph.data.frame(coords = onecoords$TR, sex = dat$sex[dat$nat=="TR"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = trgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$TR) %*% t(coefficients)
trsc <- scores [,2]

ftrsc <- trsc [which(dat$sex[dat$nat=="TR"]=="F")]
mtrsc <- trsc [which(dat$sex[dat$nat=="TR"]=="M")]

trcoef<-t(coefficients)[,2]

### uk
ukgdf <- geomorph.data.frame(coords = onecoords$UK, sex = dat$sex[dat$nat=="UK"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = ukgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$UK) %*% t(coefficients)
uksc <- scores [,2]

fuksc <- uksc [which(dat$sex[dat$nat=="UK"]=="F")]
muksc <- uksc [which(dat$sex[dat$nat=="UK"]=="M")]

ukcoef<-t(coefficients)[,2]

### vi
vigdf <- geomorph.data.frame(coords = onecoords$VIE, sex = dat$sex[dat$nat=="VIE"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = vigdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$VIE) %*% t(coefficients)
visc <- scores [,2]

fvisc <- visc [which(dat$sex[dat$nat=="VIE"]=="F")]
mvisc <- visc [which(dat$sex[dat$nat=="VIE"]=="M")]

vicoef<-t(coefficients)[,2]

### ii (IND)
iigdf <- geomorph.data.frame(coords = onecoords$IND, sex = dat$sex[dat$nat=="IND"])
reg1<-procD.lm(coords ~ sex, iter = 999, data = iigdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-cbind(sex.vecs,coefficients[2,])

scores <- two.d.array (onecoords$IND) %*% t(coefficients)
iisc <- scores [,2]

fiisc <- iisc [which(dat$sex[dat$nat=="IND"]=="F")]
miisc <- iisc [which(dat$sex[dat$nat=="IND"]=="M")]

iicoef<-t(coefficients)[,2]

## The nation specific scores concatenated into vectors of all sex scores 

fsexscores1 <- c(fbrazsc, fcmrsc, fcolsc, fczsc, fiisc, fnamsc, frosc, ftrsc, fuksc, fvisc)
msexscores1 <- c(mbrazsc, mcmrsc, mcolsc, mczsc, miisc, mnamsc, mrosc, mtrsc, muksc, mvisc)
sexscores1 <- c(fsexscores1,msexscores1)

#Check this later, each set of data is arranged differently
vioplot(sexscores1~onegdf$sex, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")
vioplot(sexscores1~onegdf$nat, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")

## Visualization of both sexes malenes-femaleness distributions
vioplot(fsexscores1~gtf$nat[gtf$sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.004, 0.004), xlab="Country",ylab="SShD")
vioplot(msexscores1~gtf$nat[gtf$sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscores1~gtf$nat, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0066",lwd=1.8,rectCol=NA,pchMed=NA)
title (main = "Sexual Shape Dimorphism")



# Correlation beween various scores and ratings

##  re-order dataset alphabetically as follows "BRAZ CMR COL CZ IND NAM RO TR UK VIE"
fdatR <- with(fdat, fdat[order(fdat$nat), ])
mdatR <- with(mdat, mdat[order(mdat$nat), ])
datR <- rbind(fdatR, mdatR)

##  scale attractiveness within each level of a factor: "nationality"
FATRA <- scale_by(Attractiveness ~ nat, data = fdatR, scale = 1)  
MATRA <- scale_by(Attractiveness ~ nat, data = mdatR, scale = 1)
SATRA <- c(FATRA, MATRA)

fdat$Attractiveness <- FATRA
mdat$Attractiveness <- MATRA
dat$Attractiveness <- SATRA

brazdat <- subset(dat, nat == "BRAZ")
fbrazdat <- subset (brazdat, sex == "F")
mbrazdat <- subset (brazdat, sex == "M")

cmrdat <- subset(dat, nat == "CMR")
fcmrdat <- subset (cmrdat, sex == "F")
mcmrdat <- subset (cmrdat, sex == "M")

coldat <- subset (dat, nat == "COL")
fcoldat <- subset (coldat, sex == "F")
mcoldat <- subset (coldat, sex == "M")

czdat <- subset (dat, nat == "CZ")
fczdat <- subset (czdat, sex == "F")
mczdat <- subset (czdat, sex == "M")

namdat <- subset (dat, nat == "NAM")
fnamdat <- subset (namdat, sex == "F")
mnamdat <- subset (namdat, sex == "M")

iidat <- subset (dat, nat == "IND")
fiidat <- subset (iidat, sex == "F")
miidat <- subset (iidat, sex == "M")

rodat <- subset (dat, nat == "RO")
frodat <- subset (rodat, sex == "F")
mrodat <- subset (rodat, sex == "M")

trdat <- subset (dat, nat == "TR")
ftrdat <- subset (trdat, sex == "F")
mtrdat <- subset (trdat, sex == "M")

ukdat <- subset (dat, nat == "UK")
fukdat <- subset (ukdat, sex == "F")
mukdat <- subset (ukdat, sex == "M")

vidat <- subset (dat, nat == "VIE")
fvidat <- subset (vidat, sex == "F")
mvidat <- subset (vidat, sex == "M")




# MIXED EFFECT MODELS

  
## Create a new variable with reversed sex scores for females - a sextypicality variable
Sextypicality <- scale(c(-1*fsexscores1, msexscores1))

## Create further variables: Asymmetry, Distinctiveness

Asymmetry <- c(fsymscores1, msymscores1)
Distinctiveness <- c(favrg, mavrg)

## Create a dataset with this variable 
dat2 <- cbind (datR, Sextypicality, Asymmetry, Distinctiveness, SATRA)
datR$nat

summary(dat2)

sum(is.na(dat2$SATRA))
dat2[is.na(dat2$SATRA),]

mean(dat2$SATRA)
sd(dat2$SATRA)


mean(dat2$Sextypicality)
sd(dat2$Sextypicality)


## Complete model testing (among other things) how does the slope for males differ form the slope for females

model1a<- lmer(SATRA ~ Sextypicality*sex + (Sextypicality*sex|nat), data = dat2)
summary(model1a)

model1b <- lmer(SATRA ~ Asymmetry*sex + (Asymmetry*sex|nat), data = dat2)
summary(model1b)

model1c <- lmer(SATRA ~ Distinctiveness*sex + (Distinctiveness*sex|nat), data = dat2)
summary(model1c)

## Complete models with all predictors

model2a<- lmer(SATRA ~ sex + age + Sextypicality + Asymmetry + Distinctiveness + sex:Sextypicality + sex:Asymmetry + sex:Distinctiveness + (sex + age + Sextypicality + Asymmetry + Distinctiveness + sex:Sextypicality + sex:Asymmetry + sex:Distinctiveness |nat), data = dat2)
summary(model2a)

# remove age
model2a<- lmer(SATRA ~ sex + Sextypicality + Asymmetry + Distinctiveness + sex:Sextypicality + sex:Asymmetry + sex:Distinctiveness + (sex + Sextypicality + Asymmetry + Distinctiveness + sex:Sextypicality + sex:Asymmetry + sex:Distinctiveness |nat), data = dat2)
summary(model2a)


model2b<- lmer(SATRA ~ sex + age + Sextypicality + Asymmetry + Distinctiveness + sex:Sextypicality + sex:Asymmetry + sex:Distinctiveness + (sex|nat) + (0+age|nat) + (0+ Sextypicality|nat) + (0+ Asymmetry|nat) + (0+Distinctiveness|nat) + (0+sex:Sextypicality|nat) + (0+sex:Asymmetry|nat) + (0+sex:Distinctiveness|nat), data = dat2)
summary(model2b)



# plot models
## distinctiveness

model<-lm(SATRA~sex*Distinctiveness, data=dat2); summary(model)

DF.pred<-expand.grid(Distinctiveness=seq(min(Distinctiveness),max(Distinctiveness),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit, x = Distinctiveness, group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Distinctiveness,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Distinctiveness, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=dat2,aes(x=Distinctiveness,y=SATRA,colour=sex, group=sex),alpha=0.05)+
  theme_bw(base_size = 14)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_blank())

DIST<-gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Distinctiveness")

DIST

##  asymmtery
model<-lm(SATRA~sex*Asymmetry, data=dat2); summary(model)

DF.pred<-expand.grid(Asymmetry=seq(min(Asymmetry),max(Asymmetry),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit, x = Asymmetry, group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Asymmetry,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Asymmetry, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=dat2,aes(x=Asymmetry,y=SATRA,colour=sex, group=sex),alpha=0.05)+
  theme_bw(base_size = 14)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_blank())

ASYM<-gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Asymmetry")

ASYM

## sextypicality

model<-lm(SATRA~sex*Sextypicality, data=dat2); summary(model)

DF.pred<-expand.grid(Sextypicality=seq(min(Sextypicality),max(Sextypicality),0.00001),sex=c("F","M"))
PRED<-predict.lm(model,newdata = DF.pred, type="response",interval="confidence")
gg.predict<-ggplot(data.frame(DF.pred,PRED),aes(y = fit, x = Sextypicality, group=sex))+
  geom_line(data=data.frame(DF.pred,PRED),aes(y = fit,x=Sextypicality,group=sex,color=sex))+
  geom_ribbon(data=data.frame(DF.pred,PRED),aes(ymin=lwr, ymax=upr, x=Sextypicality, fill = sex,group=sex), alpha = 0.2)+ geom_point(data=dat2,aes(x=Sextypicality,y=SATRA,colour=sex, group=sex),alpha=0.05)+
  theme_bw(base_size = 14)

gg.predict <- gg.predict + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_blank())

SEXTYP<-gg.predict + scale_color_discrete(labels=c("Women", "Men"))+
  scale_fill_discrete(labels=c("Women", "Men"))+
  scale_y_continuous(name="Attractiveness")+
  scale_x_continuous(name="Sextypicality")

SEXTYP




## Model restricted to the interaction term. Tests, how does the attractiveness depend on predictor in males and females separately.
model3a<- lmer(SATRA ~ 0 + Sextypicality:sex + (0 + Sextypicality:sex|nat), data = dat2)
summary(model3a)

model3b<- lmer(SATRA ~ 0 + Asymmetry:sex + (0 + Asymmetry:sex|nat), data = dat2)
summary(model3b)

model3c<- lmer(SATRA ~ 0 + Distinctiveness:sex + (0 + Distinctiveness:sex|nat), data = dat2)
summary(model3c)





# Calculate morphological variation (disparity): Morphological disparity is estimated as the Procrustes variance, overall or for groups, using residuals of a linear model fit 

mgdf <- geomorph.data.frame(coords = shapesex$M, nat = mdat$nat)
fgdf <- geomorph.data.frame(coords = shapesex$F, nat = fdat$nat)

(mdisp.m<-morphol.disparity(coords ~ 1, groups = ~nat, iter = 999, seed = NULL, data = mgdf))
(mdisp.f<-morphol.disparity(coords ~ 1, groups = ~nat, iter = 999, seed = NULL, data = fgdf))
(mdisp.1<-morphol.disparity(coords ~ 1, groups = ~nat, iter = 999, seed = NULL, data = gtf))
(mdisp.1.sex<-morphol.disparity(coords ~ sex, groups = ~nat, iter = 999, seed = NULL, data = gtf)) ## residualizing sex
(mdisp.sex<-morphol.disparity(coords ~ 1, groups = ~sex, iter = 999, seed = NULL, data = gtf)) # men are more variable in facial shape
(mdisp.sex.nat<-morphol.disparity(coords ~ nat, groups = ~sex, iter = 999, seed = NULL, data = gtf)) # men are more variable in facial shape even when controlling for nationality

setwd(folder0)
write.table(dat2,"data.clean.txt",sep="\t",row.names = F)


# QUESTIONS
# Are the more variable populations also more asymmetrical (GRAPh with diagonal like in our previous paper) but I am not sure why it should be so and maybe it would just dectract of the natural simplicity of this project. 
