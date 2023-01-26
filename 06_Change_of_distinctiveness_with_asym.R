#We load all libraries that we used to work with the raw data, but further down we focus on czech men only 
#Run from the beginning

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

#We will focus on czech men
ds<-sMshape$CZ
dsave<-Mshape$CZ

#save(ds,dsave,gpsh,file="Czech_demo_asym.Rdata")
load("Czech_demo_asym.Rdata")

asyme1<-NA
asyme2<-NA

disti1<-NA
disti2<-NA

for(spec in 1:5){
  d<-dsave
  modify<-d[,,spec]

  #Modify one target to be asymetric
  modify[6,2]<-modify[6,2]-0.01
  modify[6,1]<-modify[6,1]+0.005
  
  d<-abind(d,modify,along=3)
  
  d2D <- two.d.array(d)
  #Good, this works
  PCA<-prcomp(d2D)

  # A-SYMMETRY
  dimnames(d)[1:2]<-dimnames(gpsh$coords)[1:2]
  X<-as.data.frame(two.d.array(gpsh$coords))
  X<-as.data.frame(two.d.array(d))
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
  
  # ordering
  poradi <- order(cisla)
  
  sloupec <- paste(cisla,c(".X",".Y"),sep="")
  names(coor) <- sloupec
  coor<-coor[,poradi]
  
  symscore <-rep (0,dim(X)[1])
  for (i in 1:dim(d)[3]){
    origos <- (X) [i,]
    reflect <- (coor) [i,]
    symscore[i]<-sqrt(sum((origos-reflect)^2))
  }

  asyme1[spec]<-symscore[spec]
  asyme2[spec]<-symscore[111]
  
  #   AVERAGENESS
  ## Function for calculating distance from average
  
  dist.f.mean <- function (x) {
    lokanf<-rep(0,dim (x) [3])
    for (i in 1:dim (x) [3])
    {
      puvfanf<-(x)[,,i]
      lokanf[i]<-(sum(rowSums((puvfanf-mshape(x))^2)))^0.5
    }
    lokanf
  }
  
  avrg <- dist.f.mean(d)
  
  disti1[spec]<-avrg[spec]
  disti2[spec]<-avrg[111]
  
  print(spec)
}


GP2<-gridPar(pt.bg = "red", pt.size = 1, link.col = "blue", link.lwd = 1, link.lty = 1, out.col = "gray", out.cex = 0.1, tar.pt.bg = "black", tar.pt.size = 0.0, tar.link.col = "black", tar.link.lwd = 1, tar.link.lty = 1, tar.out.col = "black", tar.out.cex = 0.1, n.col.cell = 30, grid.col = "grey65", grid.lwd = 0.5, grid.lty = 1.5, txt.adj = 0.5, txt.pos = 1, txt.cex = 0.8, txt.col = "black")


w<-20
tiff("supp_asymmetrization.tif",width=w,height=w/2.5,units="cm",res=600,compression = "lzw")
layout(matrix(c(1,2,3,3,4,4),nrow=2),widths = c(0.5,1,1))
spec<-11
par(mar=c(0,0,0,0))
plotRefToTarget(d[,,spec], d[,,spec], method = "TPS", mag = 1, links = links, gridPars = GP2)
pu<-par("usr")
text(pu[1]+(pu[2]-pu[1])*0.1,pu[4]-(pu[4]-pu[3])*0.1,"A",font=2,xpd=NA,cex=1.2)
modify<-d[,,spec]
#Modify one target to be asymetric
modify[6,2]<-modify[6,2]-0.01
modify[6,1]<-modify[6,1]+0.005
par(mar=c(0,0,0,0))
plotRefToTarget(modify, modify, method = "TPS", mag = 1, links = links, gridPars = GP2)
pu<-par("usr")
text(pu[1]+(pu[2]-pu[1])*0.1,pu[4]-(pu[4]-pu[3])*0.1,"B",font=2,xpd=NA,cex=1.2)

pcol<-"#00A08080"
  
par(mar=c(3.5,3.5,1.5,1.5),mgp=c(2.1,0.7,0))
plot(asyme1,asyme2,xlab="Asymmetry before aymmetrization",
     ylab="Asymmetry after aymmetrization",pch=16,col=pcol,xlim=c(0.14,0.16),ylim=c(0.14,0.16))
abline(0,1,lty=2)
title("C",adj=0)
plot(disti1,disti2,xlab="Distinctiveness before aymmetrization",
     ylab="Distinctiveness after aymmetrization",pch=16,col=pcol,xlim=c(0.03,0.10),ylim=c(0.03,0.10))
abline(0,1,lty=2)
title("D",adj=0)

dev.off()


