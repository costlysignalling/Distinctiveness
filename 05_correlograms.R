#You will need rethinking package and substrRight function
library(rethinking)
library(pracma)
library(corrplot)

#Load and prepare the data
source("01_analysis.R")

#Load the model and posterior samples for visualization
load("m1.RData")
load("posterior_samples_m1.Rdata")


cor.eff<-matrix(apply(Rho,2,mean,na.rm=T),ncol=sqrt(ncol(Rho)))
rownames(cor.eff)<-c("Intercept F (a_F)","slope Sex-typicality F (bST_F)","slope Asymmetry F (bAs_F)","slope Distinctiveness F (bD_F)","Intercept M (a_M)","slope Sex-typicality M (bST_M)","slope Asymmetry M (bAs_M)","slope Distinctiveness M (bD_M)")
  
colnames(cor.eff)<-c("a_F","bST_F","bAs_F","bD_F","a_M","bST_M","bAs_M","bD_M")

tiff("corr_varying_effects_m1.tif",width=18,height=12,units="cm",res=600,compression = "lzw")
corrplot.mixed(cor.eff,upper = "ellipse",lower.col=1,tl.pos="lt",tl.col=1,diag="u")
dev.off()

#Load the model and posterior samples for visualization
load("mBMI.RData")
load("posterior_samples_mBMI.Rdata")


cor.eff<-matrix(apply(Rho,2,mean,na.rm=T),ncol=sqrt(ncol(Rho)))
rownames(cor.eff)<-c("Intercept F (a_F)","slope Sex-typicality F (bST_F)","slope Asymmetry F (bAs_F)","slope Distinctiveness F (bD_F)","slope Height F (bH_F)","slope BMI F (bBMI_F)","Intercept M (a_M)","slope Sex-typicality M (bST_M)","slope Asymmetry M (bAs_M)","slope Distinctiveness M (bD_M)","slope Height M (bH_M)","slope BMI M (bBMI_M)")

colnames(cor.eff)<-c("a_F","bST_F","bAs_F","bD_F","bH_F","bBMI_F","a_M","bST_M","bAs_M","bD_M","bH_M","bBMI_M")

tiff("corr_varying_effects_mBMI.tif",width=22,height=16,units="cm",res=600,compression = "lzw")
corrplot.mixed(cor.eff,upper = "ellipse",lower.col=1,tl.pos="lt",tl.col=1,diag="u")
dev.off()


#Load the model and posterior samples for visualization
load("mAge.RData")
load("posterior_samples_mAge.Rdata")


cor.eff<-matrix(apply(Rho,2,mean,na.rm=T),ncol=sqrt(ncol(Rho)))
rownames(cor.eff)<-c("Intercept F (a_F)","slope Sex-typicality F (bST_F)","slope Asymmetry F (bAs_F)","slope Distinctiveness F (bD_F)","slope Height F (bH_F)","slope BMI F (bBMI_F)","slope Age F (bAge_F)","Intercept M (a_M)","slope Sex-typicality M (bST_M)","slope Asymmetry M (bAs_M)","slope Distinctiveness M (bD_M)","slope Height M (bH_M)","slope BMI M (bBMI_M)","slope Age M (bAge_M)")

colnames(cor.eff)<-c("a_F","bST_F","bAs_F","bD_F","bH_F","bBMI_F","bAge_F","a_M","bST_M","bAs_M","bD_M","bH_M","bBMI_M","bAge_M")

tiff("corr_varying_effects_mAge.tif",width=22,height=16,units="cm",res=600,compression = "lzw")
corrplot.mixed(cor.eff,upper = "ellipse",lower.col=1,tl.pos="lt",tl.col=1,diag="u")
dev.off()

group.scale <- function(v,grp) {
  return((v-tapply(v,grp,mean,na.rm=T)[grp])/tapply(v,grp,sd,na.rm=T)[grp])
}

g.standardized<-data.frame(
  Attractiveness=group.scale(d.s$Attractiveness,d.s$stan.lab),
  Sextypicality=group.scale(d.s$Sextypicality,d.s$stan.lab),
  Asymmetry=group.scale(d.s$Asymmetry,d.s$stan.lab),
  Distinctiveness=group.scale(d.s$Distinctiveness,d.s$stan.lab),
  Height=group.scale(d.s$Height,d.s$stan.lab),
  BMI=group.scale(d.s$BMI,d.s$stan.lab),
  Age=group.scale(d.s$Age,d.s$stan.lab))

cor.F<-cor(g.standardized[d.s$sex=="F",],use="complete.obs")
cor.M<-cor(g.standardized[d.s$sex=="M",],use="complete.obs")

colnames(cor.F)<-c("Attr","Sex-typ.","Asym","Dist.","Height","BMI","Age")
colnames(cor.M)<-c("Attr","Sex-typ.","Asym","Dist.","Height","BMI","Age")
rownames(cor.F)[2]<-"Sex-typicality"
rownames(cor.M)<-rep("",nrow(cor.M))


my.mixed<-function (corr, lower = "number", upper = "circle", tl.pos = c("d", 
                                                                         "lt", "n"), diag = c("n", "l", "u"), bg = "white", addgrid.col = "grey", 
                    lower.col = NULL, upper.col = NULL, plotCI = c("n", "square", 
                                                                   "circle", "rect"), mar = c(0, 0, 0, 0), legend=T, ...) 
{
  tl.pos = match.arg(tl.pos)
  diag = match.arg(diag)
  plotCI = match.arg(plotCI)
  n = nrow(corr)
  adjust_plotCI = function(plotCI, method) {
    if (plotCI != "rect" || method %in% c("circle", "square")) {
      return(plotCI)
    }
    return("n")
  }
  plotCI_lower = adjust_plotCI(plotCI, lower)
  plotCI_upper = adjust_plotCI(plotCI, upper)
  oldpar = par(mar = mar, bg = "white")
  on.exit(par(oldpar), add = TRUE)
  if(legend==T){
  res1 <- corrplot(corr, type = "upper", method = upper, diag = TRUE, 
                   tl.pos = tl.pos, plotCI = plotCI_upper, col = upper.col, 
                   mar = mar, ...)
  }else{
    res1 <- corrplot(corr, type = "upper", method = upper, diag = TRUE, 
                     tl.pos = tl.pos, plotCI = plotCI_upper, col = upper.col, 
                     mar = mar, cl.pos="n", ...)
  }
    
  res2 <- corrplot(corr, add = TRUE, type = "lower", method = lower, 
                   diag = (diag == "l"), tl.pos = "n", cl.pos = "n", plotCI = plotCI_lower, 
                   col = lower.col, mar = mar, ...)
  if (diag == "n" && tl.pos != "d") {
    symbols(1:n, n:1, add = TRUE, bg = bg, fg = addgrid.col, 
            inches = FALSE, squares = rep(1, n))
  }
  corr = res1$corr
  corrPos = rbind(res1$corrPos, res2$corrPos)
  corrPos = corrPos[order(corrPos[, 1], corrPos[, 2]), ]
  res = list(corr = corr, corrPos = corrPos)
  invisible(res)
}

tiff("corr_variables.tif",width=22,height=11,units="cm",res=600,compression="lzw")
layout(matrix(c(1,2),ncol=2),widths = c(1.06,1))
my.mixed(cor.F,upper = "ellipse",lower.col=1,tl.pos="lt",tl.col=1,diag="u",legend=F)
text(1.0,8.6,"Women",cex=1.5,font=2,pos=2,xpd=NA)
my.mixed(cor.M,upper = "ellipse",lower.col=1,tl.pos="lt",tl.col=1,diag="u",legend=T,cl.offset=0.2,cl.ratio=0.3,cl.align.text="l")
text(1.5,8.7,"Men",cex=1.5,font=2,pos=2,xpd=NA)
dev.off()
