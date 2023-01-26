#You will need rethinking package and substrRight function
library(rethinking)
library(pracma)
library(scales)

#Load and prepare the data
source("01_analysis.R")

#Load the model and posterior samples for visualization
load("m1.RData")
load("posterior_samples_m1.Rdata")

ps<-c("a","bST","bAs","bD")
chars<-c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness")

#ps<-c("a","bST","bAs","bD","bH","bBMI")
#chars<-c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness","height","BMI")

#Calculate means and standard deviations for the whole sample
#Set colours for each gender
colF<-"#FFA321"
colM<-"#00AAFF"

#This returns interediate colours between gender colours and black
colF2<-c(sapply(3,colorRampPalette(c(colF,"#000000"))))[2]
colM2<-c(sapply(3,colorRampPalette(c(colM,"#000000"))))[2]

atlim<-c(1,6)

plotLin<-function(xvar,yvar,main,nat=NULL,aF,aM,bF,bM,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),cexm=1.1,pcex=0.8,palpha=0.2,xax=T,yax=T){
  
  par(mar=mar,mgp=mgp)
  
  pred<-seq(min(d[xvar],na.rm=T),max(d[xvar],na.rm=T),l=100)
  atlim<-c(1,6)
  
  plot(d[[xvar]],d[[yvar]],type="n",xlim=c(min(pred),max(pred)),ylim=atlim,xlab=ifelse(xax==T,xvar,""),ylab=ifelse(yax==T,yvar,""),axes=F,bty="n",xpd=NA)
  if(xax==T){
    axis(1,xpd=NA)
  }
  if(yax==T){
    axis(2,xpd=NA)
  }
  if(is.null(nat)){
    points(d[[xvar]][d$sex=="F"],d[[yvar]][d$sex=="F"],col=col.alpha(colF,palpha),pch=16,cex=pcex)
    points(d[[xvar]][d$sex=="M"],d[[yvar]][d$sex=="M"],col=col.alpha(colM,palpha),pch=16,cex=pcex) 
  }else{
    points(d[[xvar]][d$sex=="F" & d$nat==nat],d[[yvar]][d$sex=="F" & d$nat==nat],col=col.alpha(colF,palpha),pch=16,cex=pcex)
    points(d[[xvar]][d$sex=="M" & d$nat==nat],d[[yvar]][d$sex=="M" & d$nat==nat],col=col.alpha(colM,palpha),pch=16,cex=pcex)
  }
  
  predsF<-sapply(scaleP(pred,xvar),function(x){descaleP(aF+x*bF,yvar)})
  predsM<-sapply(scaleP(pred,xvar),function(x){descaleP(aM+x*bM,yvar)})
  
  muF<-apply(predsF,2,mean)
  CIF<-apply(predsF,2,PI)
  
  muM<-apply(predsM,2,mean)
  CIM<-apply(predsM,2,PI)
  
  shade(CIF,pred,col=col.alpha(colF2,0.25))
  shade(CIM,pred,col=col.alpha(colM2,0.25))
  
  lines(pred,muF,col=colF)
  lines(pred,muM,col=colM)
  
  title(main=main,adj=0,line=0.5,cex.main=cexm)
}



#Sextypicality

tiff("lines_Sextypicality.tif",width=16,height=12,units="cm",res=600,compression = "lzw")
layout(matrix(1:12,nrow=3,byrow = T),widths=c(1.3,1,1,1),heights = c(1,1,1.3))

plotLin("Sextypicality","Attractiveness",nat="TR",main="Turkey",aF=nwise$a_F_TR,aM=nwise$a_M_TR,bF=nwise$bST_F_TR,bM=nwise$bST_M_TR,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("Sextypicality","Attractiveness",nat="RO",main="Romania",aF=nwise$a_F_RO,aM=nwise$a_M_RO,bF=nwise$bST_F_RO,bM=nwise$bST_M_RO,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Sextypicality","Attractiveness",nat="CZ",main="Czech Republic",aF=nwise$a_F_CZ,aM=nwise$a_M_CZ,bF=nwise$bST_F_CZ,bM=nwise$bST_M_CZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Sextypicality","Attractiveness",nat="UK",main="United Kingdom",aF=nwise$a_F_UK,aM=nwise$a_M_UK,bF=nwise$bST_F_UK,bM=nwise$bST_M_UK,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)


plotLin("Sextypicality","Attractiveness",nat="IND",main="India",aF=nwise$a_F_IND,aM=nwise$a_M_IND,bF=nwise$bST_F_IND,bM=nwise$bST_M_IND,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("Sextypicality","Attractiveness",nat="VIE",main="Vietnam",aF=nwise$a_F_VIE,aM=nwise$a_M_VIE,bF=nwise$bST_F_VIE,bM=nwise$bST_M_VIE,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Sextypicality","Attractiveness",nat="COL",main="Colombia",aF=nwise$a_F_COL,aM=nwise$a_M_COL,bF=nwise$bST_F_COL,bM=nwise$bST_M_COL,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Sextypicality","Attractiveness",nat="BRAZ",main="Brazil",aF=nwise$a_F_BRAZ,aM=nwise$a_M_BRAZ,bF=nwise$bST_F_BRAZ,bM=nwise$bST_M_BRAZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)


plotLin("Sextypicality","Attractiveness",nat="NAM",main="Namibia",aF=nwise$a_F_NAM,aM=nwise$a_M_NAM,bF=nwise$bST_F_NAM,bM=nwise$bST_M_NAM,mar=c(3.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=T)
plotLin("Sextypicality","Attractiveness",nat="CMR",main="Cameroon",aF=nwise$a_F_CMR,aM=nwise$a_M_CMR,bF=nwise$bST_F_CMR,bM=nwise$bST_M_CMR,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)
plotLin("Sextypicality","Attractiveness",nat=NULL,main="Total",aF=posts$a_F_mu,aM=posts$a_M_mu,bF=posts$bST_F_mu,bM=posts$bST_M_mu,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F,palpha=0.1)

par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Women","Men"),pch=22,col=c(colF,colM),pt.bg=c(alpha(colF,0.2),alpha(colM,0.2)),bty="n",ncol=1,pt.cex=1.5,pt.lwd=1.5)

dev.off()



#Asymmetry
tiff("lines_Asymmetry.tif",width=16,height=12,units="cm",res=600,compression = "lzw")
layout(matrix(1:12,nrow=3,byrow = T),widths=c(1.3,1,1,1),heights = c(1,1,1.3))

plotLin("Asymmetry","Attractiveness",nat="TR",main="Turkey",aF=nwise$a_F_TR,aM=nwise$a_M_TR,bF=nwise$bAs_F_TR,bM=nwise$bAs_M_TR,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("Asymmetry","Attractiveness",nat="RO",main="Romania",aF=nwise$a_F_RO,aM=nwise$a_M_RO,bF=nwise$bAs_F_RO,bM=nwise$bAs_M_RO,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Asymmetry","Attractiveness",nat="CZ",main="Czech Republic",aF=nwise$a_F_CZ,aM=nwise$a_M_CZ,bF=nwise$bAs_F_CZ,bM=nwise$bAs_M_CZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Asymmetry","Attractiveness",nat="UK",main="United Kingdom",aF=nwise$a_F_UK,aM=nwise$a_M_UK,bF=nwise$bAs_F_UK,bM=nwise$bAs_M_UK,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)


plotLin("Asymmetry","Attractiveness",nat="IND",main="India",aF=nwise$a_F_IND,aM=nwise$a_M_IND,bF=nwise$bAs_F_IND,bM=nwise$bAs_M_IND,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("Asymmetry","Attractiveness",nat="VIE",main="Vietnam",aF=nwise$a_F_VIE,aM=nwise$a_M_VIE,bF=nwise$bAs_F_VIE,bM=nwise$bAs_M_VIE,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Asymmetry","Attractiveness",nat="COL",main="Colombia",aF=nwise$a_F_COL,aM=nwise$a_M_COL,bF=nwise$bAs_F_COL,bM=nwise$bAs_M_COL,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Asymmetry","Attractiveness",nat="BRAZ",main="Brazil",aF=nwise$a_F_BRAZ,aM=nwise$a_M_BRAZ,bF=nwise$bAs_F_BRAZ,bM=nwise$bAs_M_BRAZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)


plotLin("Asymmetry","Attractiveness",nat="NAM",main="Namibia",aF=nwise$a_F_NAM,aM=nwise$a_M_NAM,bF=nwise$bAs_F_NAM,bM=nwise$bAs_M_NAM,mar=c(3.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=T)
plotLin("Asymmetry","Attractiveness",nat="CMR",main="Cameroon",aF=nwise$a_F_CMR,aM=nwise$a_M_CMR,bF=nwise$bAs_F_CMR,bM=nwise$bAs_M_CMR,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)
plotLin("Asymmetry","Attractiveness",nat=NULL,main="Total",aF=posts$a_F_mu,aM=posts$a_M_mu,bF=posts$bAs_F_mu,bM=posts$bAs_M_mu,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F,palpha=0.1)

par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Women","Men"),pch=22,col=c(colF,colM),pt.bg=c(alpha(colF,0.2),alpha(colM,0.2)),bty="n",ncol=1,pt.cex=1.5,pt.lwd=1.5)

dev.off()


#Distinctiveness
tiff("lines_Distinctiveness.tif",width=16,height=12,units="cm",res=600,compression = "lzw")
layout(matrix(1:12,nrow=3,byrow = T),widths=c(1.3,1,1,1),heights = c(1,1,1.3))

plotLin("Distinctiveness","Attractiveness",nat="TR",main="Turkey",aF=nwise$a_F_TR,aM=nwise$a_M_TR,bF=nwise$bD_F_TR,bM=nwise$bD_M_TR,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("Distinctiveness","Attractiveness",nat="RO",main="Romania",aF=nwise$a_F_RO,aM=nwise$a_M_RO,bF=nwise$bD_F_RO,bM=nwise$bD_M_RO,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Distinctiveness","Attractiveness",nat="CZ",main="Czech Republic",aF=nwise$a_F_CZ,aM=nwise$a_M_CZ,bF=nwise$bD_F_CZ,bM=nwise$bD_M_CZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Distinctiveness","Attractiveness",nat="UK",main="United Kingdom",aF=nwise$a_F_UK,aM=nwise$a_M_UK,bF=nwise$bD_F_UK,bM=nwise$bD_M_UK,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)


plotLin("Distinctiveness","Attractiveness",nat="IND",main="India",aF=nwise$a_F_IND,aM=nwise$a_M_IND,bF=nwise$bD_F_IND,bM=nwise$bD_M_IND,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("Distinctiveness","Attractiveness",nat="VIE",main="Vietnam",aF=nwise$a_F_VIE,aM=nwise$a_M_VIE,bF=nwise$bD_F_VIE,bM=nwise$bD_M_VIE,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Distinctiveness","Attractiveness",nat="COL",main="Colombia",aF=nwise$a_F_COL,aM=nwise$a_M_COL,bF=nwise$bD_F_COL,bM=nwise$bD_M_COL,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("Distinctiveness","Attractiveness",nat="BRAZ",main="Brazil",aF=nwise$a_F_BRAZ,aM=nwise$a_M_BRAZ,bF=nwise$bD_F_BRAZ,bM=nwise$bD_M_BRAZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)


plotLin("Distinctiveness","Attractiveness",nat="NAM",main="Namibia",aF=nwise$a_F_NAM,aM=nwise$a_M_NAM,bF=nwise$bD_F_NAM,bM=nwise$bD_M_NAM,mar=c(3.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=T)
plotLin("Distinctiveness","Attractiveness",nat="CMR",main="Cameroon",aF=nwise$a_F_CMR,aM=nwise$a_M_CMR,bF=nwise$bD_F_CMR,bM=nwise$bD_M_CMR,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)
plotLin("Distinctiveness","Attractiveness",nat=NULL,main="Total",aF=posts$a_F_mu,aM=posts$a_M_mu,bF=posts$bD_F_mu,bM=posts$bD_M_mu,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F,palpha=0.1)

par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Women","Men"),pch=22,col=c(colF,colM),pt.bg=c(alpha(colF,0.2),alpha(colM,0.2)),bty="n",ncol=1,pt.cex=1.5,pt.lwd=1.5)

dev.off()



#Here I will switch to a different model, beause I need slopes for Height and BMI
load("mBMI.RData")
load("posterior_samples_mBMI.Rdata")

#Height
tiff("lines_Height.tif",width=16,height=12,units="cm",res=600,compression = "lzw")
layout(matrix(1:12,nrow=3,byrow = T),widths=c(1.3,1,1,1),heights = c(1,1,1.3))

plotLin("height","Attractiveness",nat="TR",main="Turkey",aF=nwise$a_F_TR,aM=nwise$a_M_TR,bF=nwise$bH_F_TR,bM=nwise$bH_M_TR,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("height","Attractiveness",nat="RO",main="Romania",aF=nwise$a_F_RO,aM=nwise$a_M_RO,bF=nwise$bH_F_RO,bM=nwise$bH_M_RO,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("height","Attractiveness",nat="CZ",main="Czech Republic",aF=nwise$a_F_CZ,aM=nwise$a_M_CZ,bF=nwise$bH_F_CZ,bM=nwise$bH_M_CZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)

xax=F;yax=F;par(mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0))
plot(d[["height"]],d[["Attractiveness"]],type="n",xlim=c(min(d$height,na.rm=T),max(d$height,na.rm=T)),ylim=atlim,xlab=ifelse(xax==T,xvar,""),ylab=ifelse(yax==T,"Attractiveness",""),axes=F,bty="n",xpd=NA)
pu<-par("usr")
text((pu[2]+pu[1])/2,(pu[4]+pu[3])/2,"NA",cex=1.5)
title(main="United Kingdom",adj=0,line=0.5,cex.main=1.1)


xax=F;yax=T;par(mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0))
plot(d[["height"]],d[["Attractiveness"]],type="n",xlim=c(min(d$height,na.rm=T),max(d$height,na.rm=T)),ylim=atlim,xlab=ifelse(xax==T,xvar,""),ylab=ifelse(yax==T,"Attractiveness",""),axes=F,bty="n",xpd=NA)
axis(2,xpd=NA)
pu<-par("usr")
text((pu[2]+pu[1])/2,(pu[4]+pu[3])/2,"NA",cex=1.5)
title(main="India",adj=0,line=0.5,cex.main=1.1)
plotLin("height","Attractiveness",nat="VIE",main="Vietnam",aF=nwise$a_F_VIE,aM=nwise$a_M_VIE,bF=nwise$bH_F_VIE,bM=nwise$bH_M_VIE,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("height","Attractiveness",nat="COL",main="Colombia",aF=nwise$a_F_COL,aM=nwise$a_M_COL,bF=nwise$bH_F_COL,bM=nwise$bH_M_COL,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("height","Attractiveness",nat="BRAZ",main="Brazil",aF=nwise$a_F_BRAZ,aM=nwise$a_M_BRAZ,bF=nwise$bH_F_BRAZ,bM=nwise$bH_M_BRAZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)


plotLin("height","Attractiveness",nat="NAM",main="Namibia",aF=nwise$a_F_NAM,aM=nwise$a_M_NAM,bF=nwise$bH_F_NAM,bM=nwise$bH_M_NAM,mar=c(3.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=T)
plotLin("height","Attractiveness",nat="CMR",main="Cameroon",aF=nwise$a_F_CMR,aM=nwise$a_M_CMR,bF=nwise$bH_F_CMR,bM=nwise$bH_M_CMR,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)
plotLin("height","Attractiveness",nat=NULL,main="Total",aF=posts$a_F_mu,aM=posts$a_M_mu,bF=posts$bH_F_mu,bM=posts$bH_M_mu,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F,palpha=0.1)

par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Women","Men"),pch=22,col=c(colF,colM),pt.bg=c(alpha(colF,0.2),alpha(colM,0.2)),bty="n",ncol=1,pt.cex=1.5,pt.lwd=1.5)
dev.off()


#BMI
tiff("lines_BMI.tif",width=16,height=12,units="cm",res=600,compression = "lzw")
layout(matrix(1:12,nrow=3,byrow = T),widths=c(1.3,1,1,1),heights = c(1,1,1.3))

plotLin("BMI","Attractiveness",nat="TR",main="Turkey",aF=nwise$a_F_TR,aM=nwise$a_M_TR,bF=nwise$bBMI_F_TR,bM=nwise$bBMI_M_TR,mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=T)
plotLin("BMI","Attractiveness",nat="RO",main="Romania",aF=nwise$a_F_RO,aM=nwise$a_M_RO,bF=nwise$bBMI_F_RO,bM=nwise$bBMI_M_RO,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("BMI","Attractiveness",nat="CZ",main="Czech Republic",aF=nwise$a_F_CZ,aM=nwise$a_M_CZ,bF=nwise$bBMI_F_CZ,bM=nwise$bBMI_M_CZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)

xax=F;yax=F;par(mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0))
plot(d[["BMI"]],d[["Attractiveness"]],type="n",xlim=c(min(d$BMI,na.rm=T),max(d$BMI,na.rm=T)),ylim=atlim,xlab=ifelse(xax==T,xvar,""),ylab=ifelse(yax==T,"Attractiveness",""),axes=F,bty="n",xpd=NA)
pu<-par("usr")
text((pu[2]+pu[1])/2,(pu[4]+pu[3])/2,"NA",cex=1.5)
title(main="United Kingdom",adj=0,line=0.5,cex.main=1.1)


xax=F;yax=T;par(mar=c(0.5,3.5,3,0.5),mgp=c(2,0.7,0))
plot(d[["BMI"]],d[["Attractiveness"]],type="n",xlim=c(min(d$BMI,na.rm=T),max(d$BMI,na.rm=T)),ylim=atlim,xlab=ifelse(xax==T,xvar,""),ylab=ifelse(yax==T,"Attractiveness",""),axes=F,bty="n",xpd=NA)
axis(2,xpd=NA)
pu<-par("usr")
text((pu[2]+pu[1])/2,(pu[4]+pu[3])/2,"NA",cex=1.5)
title(main="India",adj=0,line=0.5,cex.main=1.1)
plotLin("BMI","Attractiveness",nat="VIE",main="Vietnam",aF=nwise$a_F_VIE,aM=nwise$a_M_VIE,bF=nwise$bBMI_F_VIE,bM=nwise$bBMI_M_VIE,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("BMI","Attractiveness",nat="COL",main="Colombia",aF=nwise$a_F_COL,aM=nwise$a_M_COL,bF=nwise$bBMI_F_COL,bM=nwise$bBMI_M_COL,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=F,yax=F)
plotLin("BMI","Attractiveness",nat="BRAZ",main="Brazil",aF=nwise$a_F_BRAZ,aM=nwise$a_M_BRAZ,bF=nwise$bBMI_F_BRAZ,bM=nwise$bBMI_M_BRAZ,mar=c(0.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)


plotLin("BMI","Attractiveness",nat="NAM",main="Namibia",aF=nwise$a_F_NAM,aM=nwise$a_M_NAM,bF=nwise$bBMI_F_NAM,bM=nwise$bBMI_M_NAM,mar=c(3.5,3.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=T)
plotLin("BMI","Attractiveness",nat="CMR",main="Cameroon",aF=nwise$a_F_CMR,aM=nwise$a_M_CMR,bF=nwise$bBMI_F_CMR,bM=nwise$bBMI_M_CMR,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F)
plotLin("BMI","Attractiveness",nat=NULL,main="Total",aF=posts$a_F_mu,aM=posts$a_M_mu,bF=posts$bBMI_F_mu,bM=posts$bBMI_M_mu,mar=c(3.5,0.5,3,0.5),mgp=c(2,0.7,0),xax=T,yax=F,palpha=0.1)

par(mar=c(0,0,0,0))
plot(NULL,ylim=c(0,1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
legend("center",c("Women","Men"),pch=22,col=c(colF,colM),pt.bg=c(alpha(colF,0.2),alpha(colM,0.2)),bty="n",ncol=1,pt.cex=1.5,pt.lwd=1.5)
dev.off()

