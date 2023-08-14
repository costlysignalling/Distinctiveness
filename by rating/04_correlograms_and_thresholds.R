#You will need rethinking package and substrRight function
library(rethinking)
library(pracma)
library(corrplot)

#Load and prepare the data
source("01_data_by_rating_prepare.R")

#Load the model and posterior samples for visualization
load("m1_by_rating.RData")
load("posterior_samples_m1.Rdata")


cor.eff<-matrix(apply(Rho,2,mean,na.rm=T),ncol=sqrt(ncol(Rho)))
rownames(cor.eff)<-c("Intercept F (a_F)","slope Sex-typicality F (bST_F)","slope Asymmetry F (bAs_F)","slope Distinctiveness F (bD_F)","Intercept M (a_M)","slope Sex-typicality M (bST_M)","slope Asymmetry M (bAs_M)","slope Distinctiveness M (bD_M)")

colnames(cor.eff)<-c("a_F","bST_F","bAs_F","bD_F","a_M","bST_M","bAs_M","bD_M")

tiff("corr_varying_effects_m1_by_rating.tif",width=18,height=12,units="cm",res=600,compression = "lzw")
corrplot.mixed(cor.eff,upper = "ellipse",lower.col=1,tl.pos="lt",tl.col=1,diag="u")
dev.off()




all.params<-read.table("params_alphas_m1.txt",sep="\t",header = T)

hex<-all.params$hex[match(unique(all.params$col),all.params$col)]
cols<-all.params$col[match(unique(all.params$col),all.params$col)]

alpha<-"80"
all.params$hex<-paste("#",hex[match(all.params$col,cols)],alpha,sep="")

de<-all.params

de$n<-1:nrow(de)
de$ofs[is.na(de$ofs)]<-0

de$y<-de$n+de$block-1+cumsum(de$ofs)

#constant to add on both sides of the y axis to expand the plotting region a bit
const1<-1
#constant that is there for an extra space for axis
const2<-1.8

db<-lapply(1:max(de$block),function(i){de[de$block==i,]})
ploth<-sapply(1:length(db),function(i){max(db[[i]]$y)-min(db[[i]]$y)})
rath<-c(const2,ploth+2*const1+const2)


alpha_prior<-alphas
for(i in 1:ncol(alpha_prior)){
  alpha_prior[,i]<-rnorm(nrow(alpha_prior),0,1.5)
}


tiff("alphas_m1.tif",width=12,height=7.5,units="cm",res=600,compression = "lzw")

par(oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:6,nrow=2,ncol=3),widths=c(1.3,1,1),heights=c(0.15,1))

axes<-list(seq(-5,5,l=5))

xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})


par(mar=c(0.05,0.2,0.05,0.2),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.9,"Intercepts of ordered-logit\nlink function",pos=4,font=2,xpd=NA)

dei<-db[[1]]
block<-1

plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,dei$y,paste(dei$predictor,"(",round(colMeans(alphas),2),")"),pos=4)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Posterior",pos=4,font=2)
plotMi(alphas,axis=T,area.scale=0.01,col.bg = "#FCFCEF")

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Prior",pos=4,font=2)
plotMi(alpha_prior,axis=T,area.scale=0.1,col.bg = "#FCFCEF")

dev.off()
