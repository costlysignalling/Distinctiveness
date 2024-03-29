#You will need rethinking package and substrRight function
library(rethinking)
library(pracma)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Load and prepare data
source("01_data_by_rating_prepare.R")
#Load estimated posterior for visualization
load("m1_by_rating.Rdata")
load("m1.prior.Rdata")

#Check sample sizes and alignment of factor levels in nationality and sex
table(as.integer(as.factor(d$nat)),d$nat)
table(as.integer(as.factor(d$sex)),d$sex)

#Summarize the model posterior
(prec.fit<-precis(m1,depth=3))

#Extract samples for visualization
post<-extract.samples(m1)

#Check the resulting structure
str(post)

#Names of the matrices
nam<-names(post)

#Names of crucial model parameters
parnames<-nam[substrRight(nam,2)=="mu"]
labnames<-substr(parnames,1,nchar(parnames)-2) #without the mu - the mu is only the mean

#If the UK sample is discarded, we have to update the nationality variable
dl$nat<-as.factor(as.character(dl$nat))
levels(dl$nat)
summary(dl$nat)

dl$nat<-factor(dl$nat,levels = c("NAM","CMR","TR","RO","CZ","UK","BRAZ","VIE"))
summary(as.factor(dl$nat))

#Create labels for varying intercepts and slopes
labs<-paste(rep(labnames,each=dim(post$pars)[2],times=2),rep(c("F","M"),each=length(labnames)*dim(post$pars)[2]),rep(levels(dl$nat),times=length(labnames)*2),sep="_")
labs


#Extract the national part of labels for easy sorting pourposes
labnat<-sapply(labs,function(x)(strsplit(x,"_")[[1]][3]))

#Extract posterior samples per varying parameter per nation as a matrix one row per each sample
parsmat<-matrix(unlist(post$pars),nrow=dim(post$pars)[1])

#Check if it worked
dim(parsmat)
str(post)

#Check parameter means if they fit the label arrangement
cm<-colMeans(parsmat)
plot(cm)

#Check if the order of the labels fit these estimations
labs

#Extract varying slope and intercept means
eval(parse(text=(
  paste("posts<-cbind(",paste(paste("post$",nam[substrRight(nam,2)=="mu"],sep=""),collapse=","),")"))))

#Extract priors
eval(parse(text=(
  paste("prios<-cbind(",paste(paste("prio$",nam[substrRight(nam,2)=="mu"],sep=""),collapse=","),")"))))

#The original extracted prior is not for the rating model
str(prios)
for(i in 1:ncol(prios)){
  prios[,i]<-rnorm(nrow(prios),0,2.5)
}

#Distribution of parameter Rho
Rho.prior<-matrix(unlist(prio$Rho),nrow=dim(prio$Rho)[1])
str(Rho.prior)
Rho.not.one<-Rho.prior[,-c(0:7*8+1:8)]
quantile(abs(Rho.not.one),0.89)

#Create labs for mean parameter values
labs2<-paste(rep(labnames,each=2),rep(c("F","M"),times=length(labnames)),"mu",sep="_")

#Extract sigmas of varying slopes and intercepts as a matrix one row per each sample and Standard deviation of the likelihood function around the linear model
s_vary<-cbind(post$sigma_r,post$sigma_t,post$sigma_nat)
s_vary_prior<-cbind(prio$sigma_r,prio$sigma_t,prio$sigma_nat)

labs3<-c(c("rater_a_s","rater_bST_s","rater_bAs_s","rater_bD_s"),"target_s",paste(rep(labnames,times=2),rep(c("F","M"),each=length(labnames)),"s",sep="_"))

#Posterior samples of correlations between varying slopes and intercepts
Rho<-matrix(unlist(post$Rho),nrow=dim(post$Rho)[1])
Rho_prior<-matrix(unlist(prio$Rho),nrow=dim(prio$Rho)[1])

#Create labs per correlation coefficient samples
slopes<-paste(rep(labnames,times=2),rep(c("F","M"),each=length(labnames)),sep="_")
labs4<-paste(rep(slopes,each=length(slopes)),",",rep(slopes,times=length(slopes)),sep="")

#Thresholds of the ordered-ogit function
alphas<-post$alpha
alphas<-as.data.frame(alphas)
names(alphas)<-paste("threshold",c("1-2","2-3","3-4","4-5","5-6","6-7"))

#Convers samples posterior into data frame format
posts<-as.data.frame(posts)
nwise<-as.data.frame(parsmat)
s_vary<-as.data.frame(s_vary)
s_vary_prior<-as.data.frame(s_vary_prior)
Rho<-as.data.frame(Rho)
Rho_prior<-as.data.frame(Rho_prior)

#Label the columns properly
names(nwise)<-labs
names(posts)<-labs2
names(s_vary)<-labs3
#names(s_vary_prior)<-labs3
names(Rho)<-labs4
names(Rho_prior)<-labs4


#Check whether samples are aligned properly, if mean estimates check
round(apply(nwise,2,mean),2)
round(apply(posts,2,mean),2)
round(apply(s_vary,2,mean),2)
round(apply(Rho,2,mean),2)


#Create labs for visualization pourposes
plotlabs<-unlist(substr(names(posts),1,nchar(names(posts))-3))

write.table(data.frame(nam=plotlabs),"params_empty.txt",row.names=F,sep="\t")


#Rearrange columns in posterior estimates by-nation
re.ar<-rep(1:(ncol(posts)/2),each=2)+rep(c(0,ncol(posts)/2),times=ncol(posts)/2)
postsn<-lapply(levels(dl$nat),function(n){nwise[,labnat==n][,re.ar]})
str(postsn)


all.params<-read.table("params_m1.txt",sep="\t",header = T)

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
rath<-c(rath,rath[2],rath[2])

rat<-c(1.2,1,1,1,1)

laymat<-matrix(c(1:20),nr=4)

tiff("posterior_m1.tif",width=20,height=16,units="cm",res=600,compression = "lzw")

par(oma=c(0.5,0.5,0.5,0.5))
layout(laymat,widths=rat,heights=rath)

#axes<-list(seq(-0.8,0.8,l=5))
axes<-list(seq(-10,10,l=5))

xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})

par(mar=c(0.05,0.2,0.05,0.2),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.3,"Attractiveness ~",pos=4,font=2)

dei<-db[[1]]
block<-1

for(i in 1:3){
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  text(0,dei$y,paste(dei$predictor,dei$condition),pos=4)
  text(0,max(dei$y)+const1+const2-0.3,ifelse(i==3,"","Attractiveness ~"),pos=4,font=2,xpd=NA)
}


plotMi<-function(posts,col.bg="white",axis=T,below="",area.scale=0.15){
  
  plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlim=xlims[[block]],bty="n",xlab="",ylab="")
  
  #Draw background
  rect(-100,0,100,max(dei$y)+1,border=NA,col=col.bg)
  #Outline places for plotting semitransparent violin plots
  
  #density areas are scaled within each block.
  area<-area.scale*diff(xlims[[block]])
  
  for(i in 1:nrow(dei)){
    thispost<-posts[,dei$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col="white",border=NA)
  }
  
  col.grid<-"#808080"
  lwd.grid<-1.5
  abline(h=dei$y,col=col.grid,lty=1,lwd=lwd.grid)
  
  lwd.ax<-1.5
  col.ax<-"#202020"
  
  if(axis==T){
    tic<-0.35
    ofs<-0.65
    segments(axes[[block]],max(dei$y)+const1,axes[[block]],max(dei$y)+tic+const1,lwd=lwd.ax,col=col.ax)
    text(axes[[block]],max(dei$y)+tic+const1+ofs,labels=axes[[block]],col=col.ax,cex=0.9,font=2)
    lines(range(axes[[block]]),rep(max(dei$y)+const1,2),lwd=lwd.ax,col=col.ax)
  }else{
    text(xlims[[block]][1],max(dei$y)+const1+const2-0.5,below,pos=4,font=2,xpd=T)
  }
  
  lwd.v<-1.5
  segments(axes[[block]],min(dei$y)-const1,axes[[block]],max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=3)
  segments(0,min(dei$y)-const1,0,max(dei$y)+const1,lwd=lwd.v,col=col.ax,lty=1)
  
  #Draw density polygons
  lwd.pol<-1.5
  col.pol<-col.grid
  
  for(i in 1:nrow(dei)){
    thispost<-posts[,dei$n[i]]
    dens<-density(thispost)
    polX<-c(dens$x,rev(dens$x))
    polY<-c(dens$y,rev(-dens$y))
    ar1<-abs(polyarea(polX,polY))
    perc<-area/ar1
    polygon(polX,polY*perc+dei$y[i],col=dei$hex[i],border=col.pol,lwd=lwd.pol)
    CI<-PI(thispost,prob = 0.89)
    lines(CI,rep(dei$y[i],2),lwd=2,col="#000000")
    points(mean(thispost),dei$y[i],pch=16,cex=0.9,col="#FFFFFF")
    points(mean(thispost),dei$y[i],pch=1,cex=0.9,col="#000000")
  }
  rect(xlims[[block]][1],min(dei$y)-const1,xlims[[block]][2],max(dei$y)+const1,lwd=lwd.ax,border=col.ax,xpd=T)
}


#Check national levels for correct ordering of plot panels
levels(dl$nat)

#Background colors per continents: Africa, Europe, America, Asia
colcont<-c("#DDEEFF","#FFDDEE","#DDEEDD","#F0F0DD")

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Turkey",pos=4,font=2)
plotMi(postsn[[3]],axis=F,below="India",col.bg=colcont[2])

plot(NULL,ylim=c(max(dei$y)+const1+const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0.5,(max(dei$y)+const1+const2)/2,"NA",cex=1.5)
text(0,max(dei$y)+const1+const2-0.5,"Namibia",pos=4,font=2)

plotMi(postsn[[1]],axis=T,col.bg=colcont[1])

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Romania",pos=4,font=2)
plotMi(postsn[[4]],axis=F,below="Vietnam",col.bg=colcont[2])
plotMi(postsn[[8]],axis=F,below="Cameroon",col.bg=colcont[4])
plotMi(postsn[[2]],axis=T,col.bg=colcont[1])

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Czech Republic",pos=4,font=2)
plotMi(postsn[[5]],axis=F,below="Colombia",col.bg=colcont[2])

plot(NULL,ylim=c(max(dei$y)+const1+const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0.5,(max(dei$y)+const1+const2)/2,"NA",cex=1.5)
text(0,max(dei$y)+const1+const2-0.5,"Total",pos=4,font=2)

plotMi(posts,axis=T)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"United Kingdom",pos=4,font=2)
plotMi(postsn[[6]],axis=F,below="Brazil",col.bg=colcont[2])
plotMi(postsn[[7]],axis=F,below="Prior (Total)",col.bg=colcont[3])
plotMi(prios,axis=T)
dev.off()

#Write parameter estimates table into a .txt file
postsall<-postsn
postsall[[length(postsn)+1]]<-posts

param.names<-sapply(names(posts),function(x){paste(strsplit(x,"_")[[1]][1:2],collapse="_")})

estimates<-t(sapply(1:length(postsall),function(i){paste(format(round(sapply(postsall[[i]],mean),2),nsmall=2)," [",apply(format(round(sapply(postsall[[i]],PI,prob=0.89),2),nsmall=2),2,paste,collapse=", "),"]",sep="")}))

colnames(estimates)<-param.names
rownames(estimates)<-c(levels(dl$nat),"Total")

write.table(estimates,"Parameter_estimates_m1.txt",sep="\t",col.names=NA)

#We will harvest the full power of Bazesian analysis and generate also tables that the effect is positive and that the effect is small.
pro.pos.big<-t(sapply(1:length(postsall),function(i){paste(
  format(round(sapply(postsall[[i]],function(v){sum(v>0)/length(v)}),2),nsmall=2)," (",
  format(round(sapply(postsall[[i]],function(v){sum(abs(v)>0.1)/length(v)}),2),nsmall=2),")",sep="")}))

colnames(pro.pos.big)<-param.names
rownames(pro.pos.big)<-c(levels(dl$nat),"Total")

write.table(pro.pos.big,"Parameter_probabilities_m1.txt",sep="\t",col.names=NA)


#Deltas - differences between parameter estimates
deltas<-lapply(postsall,function(sub){data.frame(
  a=sub[[1]]-sub[[2]],
  bST=sub[[3]]-sub[[4]],
  bAs=sub[[5]]-sub[[6]],
  bD=sub[[7]]-sub[[8]])})

tab.deltas<-t(sapply(1:length(deltas),function(i){paste(format(round(sapply(deltas[[i]],mean),2),nsmall=2)," [",apply(format(round(sapply(deltas[[i]],PI,prob=0.89),2),nsmall=2),2,paste,collapse=", "),"]",sep="")}))

colnames(tab.deltas)<-c("a[women]-a[men]","bST[women]-bST[men]","bAs[women]-bAs[men]","bD[women]-bD[men]")
rownames(tab.deltas)<-c(levels(dl$nat),"Total")

write.table(tab.deltas,"deltas_sex_m1.txt",sep="\t",col.names=NA)

#Sigma summary
s_vary<-s_vary[,c(1:5,5+rep(c(0,4),times=(ncol(s_vary)-5)/2)+rep(1:((ncol(s_vary)-5)/2),each=2))]

s_vary_prior<-s_vary

#The original extracted prior is not for the rating model
for(i in 1:ncol(s_vary_prior)){
  s_vary_prior[,i]<-rexp(nrow(s_vary_prior),1)
}

# s_vary_prior<-s_vary_prior[,c(1:5,5+rep(c(0,4),times=(ncol(s_vary)-6)/2)+rep(1:((ncol(s_vary)-6)/2),each=2),length(s_vary))]

write.table(data.frame(nam=names(s_vary)),"params_empty.txt",row.names=F,sep="\t")

all.params<-read.table("params_sigmas_m1.txt",sep="\t",header = T)

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


tiff("sigmas_m1.tif",width=12,height=8.5,units="cm",res=600,compression = "lzw")

par(oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:6,nrow=2,ncol=3),widths=c(1.3,1,1),heights=c(0.15,1))

axes<-list(seq(0,15,l=4))

xlims<-lapply(1:length(axes),function(i){c(min(axes[[i]])-diff(range(axes[[i]]))*0.1,max(axes[[i]])+diff(range(axes[[i]]))*0.1)})


par(mar=c(0.05,0.2,0.05,0.2),cex=0.9)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.9,"SD varying effects",pos=4,font=2,xpd=NA)

dei<-db[[1]]
block<-1

plot(NULL,ylim=c(max(dei$y)+const1+const2,min(dei$y)-const1),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,dei$y,paste(dei$predictor,dei$condition),pos=4)

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Posterior",pos=4,font=2)
plotMi(s_vary,axis=T,area.scale=0.1,col.bg = "#FCFCEF")

plot(NULL,ylim=c(const2,0),xlim=c(0,1),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
text(0,const2-0.5,"Prior",pos=4,font=2)
plotMi(s_vary_prior,axis=T,area.scale=0.1,col.bg = "#FCFCEF")

dev.off()


#Make sigma result table
sigmas<-lapply(s_vary,function(x){paste(format(round(mean(x),2),nsmall=2)," [",format(round(PI(x,prob=0.89)[1],2),nsmall=2),",",format(round(PI(x,prob=0.89)[2],2),nsmall=2),"]",sep="")})

sig.tab<-as.data.frame(do.call(rbind,sigmas))
names(sig.tab)<-"Mean [89% CI]"

write.table(sig.tab,"Sigma_estimates_m1.txt",sep="\t",col.names=NA)


#Explained variance
explained<-1-s_vary[[length(s_vary)]]^2
round(mean(explained),2)
round(PI(explained),2)

#Worst convergence measures
prec.fit
min(prec.fit$n_eff,na.rm=T)
max(prec.fit$Rhat,na.rm=T)

save(posts,postsn,nwise,s_vary,Rho,alphas,file="posterior_samples_m1.Rdata")
