start<-Sys.time()

library(rethinking)

#Load the data
source("01_analysis.R")

#See by sample counts and level orderings
table(as.integer(as.factor(d$nat)),d$nat)
table(as.integer(as.factor(d$sex)),d$sex)

sum1<-as.data.frame(t(summary(d$nat)))
size1<-cbind(sum1,Total=sum(sum1),Mean=round(mean(unlist(sum1)),2),SD=round(sd2(unlist(sum1)),2))

sum1M<-as.data.frame(t(summary(d$nat[d$sex=="M"])))
size1M<-cbind(sum1M,Total=sum(sum1M),Mean=round(mean(unlist(sum1M)),2),SD=round(sd2(unlist(sum1M)),2))
sum1F<-as.data.frame(t(summary(d$nat[d$sex=="F"])))
size1F<-cbind(sum1F,Total=sum(sum1F),Mean=round(mean(unlist(sum1F)),2),SD=round(sd2(unlist(sum1F)),2))

d.list<-list(Attractiveness=d.s$Attractiveness,
             Sextypicality=d.s$Sextypicality,
             Asymmetry=d.s$Asymmetry,
             Distinctiveness=d.s$Distinctiveness,
             sex=as.integer(d.s$sex),
             nat=as.integer(d.s$nat))

#In model comparison with BMI, we cannot use UK and IND sample, where body weight information is missing  at least in one gender
nrow(d.s)
d.s2<-d.s[!is.na(d.s$Height),]
nrow(d.s2)
d.s2<-d.s2[!is.na(d.s2$BMI),]
nrow(d.s2)

#We also remove UK men, since they lack female counterparts

summary(as.factor(paste(d.s2$nat,d.s2$sex)))
d.s2<-d.s2[d.s2$nat!="UK",]

summary(d.s2$nat)
d.s2$nat<-factor(d.s2$nat,levels = c("NAM","CMR","TR","RO","CZ","COL","BRAZ","VIE"))
summary(d.s2$nat)

table(as.integer(d.s2$nat),d.s2$nat)

sum2<-as.data.frame(t(summary(d.s2$nat)))
size2<-cbind(sum2,Total=sum(sum2),Mean=round(mean(unlist(sum2)),2),SD=round(sd2(unlist(sum2)),2))

sum2M<-as.data.frame(t(summary(d.s2$nat[d.s2$sex=="M"])))
size2M<-cbind(sum2M,Total=sum(sum2M),Mean=round(mean(unlist(sum2M)),2),SD=round(sd2(unlist(sum2M)),2))
sum2F<-as.data.frame(t(summary(d.s2$nat[d.s2$sex=="F"])))
size2F<-cbind(sum2F,Total=sum(sum2F),Mean=round(mean(unlist(sum2F)),2),SD=round(sd2(unlist(sum2F)),2))

size<-rbind(size1,size1F,size1M,
            unlist(size2)[match(names(size1),names(size2))],
            unlist(size2F)[match(names(size1),names(size2))],
            unlist(size2M)[match(names(size1),names(size2))])

rownames(size)<-c("Main model","Women (Main)","Men (Main)","Extended model","Women (Extended)","Men (Extended)")

write.table(size,file="sample.size.txt",sep="\t",col.names=NA)

d.list2<-list(Attractiveness=d.s2$Attractiveness,
             Sextypicality=d.s2$Sextypicality,
             Asymmetry=d.s2$Asymmetry,
             Distinctiveness=d.s2$Distinctiveness,
             Height=d.s2$Height,
             BMI=d.s2$BMI,
             sex=as.integer(d.s2$sex),
             nat=as.integer(d.s2$nat))

#Fit simple model with all the data
m1<-ulam(
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- pars[nat,(sex-1)*4+1]+pars[nat,(sex-1)*4+2]*Sextypicality+pars[nat,(sex-1)*4+3]*Asymmetry+pars[nat,(sex-1)*4+4]*Distinctiveness,
    
    vector[8]:pars[nat] ~ multi_normal(c(amu[1],bSTmu[1],bAsmu[1],bDmu[1],amu[2],bSTmu[2],bAsmu[2],bDmu[2]),Rho,sigma_nat),
    
    amu[sex] ~ normal(0,0.2),
    bSTmu[sex] ~ normal(0,0.5),
    bAsmu[sex] ~ normal(0,0.5),
    bDmu[sex] ~ normal(0,0.5),
    
    sigma_nat ~ exponential(1),
    sigma ~ exponential(1),
    
    Rho ~ lkj_corr(2)
  ), data=d.list , chains=10 , cores=10, log_lik=TRUE ,iter = 8000, control=list(max_treedepth=18,adapt_delta=0.99))
save(m1,file="m1.Rdata")

prio<-extract.prior(m1,n=10000)
save(prio,file="m1.prior.Rdata")

#The same model with the subset of the data that include info on body height and BMI
m1.BMI<-ulam(
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- pars[nat,(sex-1)*4+1]+pars[nat,(sex-1)*4+2]*Sextypicality+pars[nat,(sex-1)*4+3]*Asymmetry+pars[nat,(sex-1)*4+4]*Distinctiveness,
    
    vector[8]:pars[nat] ~ multi_normal(c(amu[1],bSTmu[1],bAsmu[1],bDmu[1],amu[2],bSTmu[2],bAsmu[2],bDmu[2]),Rho,sigma_nat),
    
    amu[sex] ~ normal(0,0.2),
    bSTmu[sex] ~ normal(0,0.5),
    bAsmu[sex] ~ normal(0,0.5),
    bDmu[sex] ~ normal(0,0.5),
    
    sigma_nat ~ exponential(1),
    sigma ~ exponential(1),
    
    Rho ~ lkj_corr(2)
  ), data=d.list2 , chains=10 , cores=10, log_lik=TRUE ,iter = 8000, control=list(max_treedepth=18,adapt_delta=0.99))
save(m1.BMI,file="m1.BMI.Rdata")

#Model including body height and BMI
mBMI<-ulam(
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- pars[nat,(sex-1)*6+1]+pars[nat,(sex-1)*6+2]*Sextypicality+pars[nat,(sex-1)*6+3]*Asymmetry+pars[nat,(sex-1)*6+4]*Distinctiveness+pars[nat,(sex-1)*6+5]*Height+pars[nat,(sex-1)*6+6]*BMI,
    
    vector[12]:pars[nat] ~ multi_normal(c(amu[1],bSTmu[1],bAsmu[1],bDmu[1],bHmu[1],bBMImu[1],amu[2],bSTmu[2],bAsmu[2],bDmu[2],bHmu[2],bBMImu[2]),Rho,sigma_nat),
    
    amu[sex] ~ normal(0,0.2),
    bSTmu[sex] ~ normal(0,0.5),
    bAsmu[sex] ~ normal(0,0.5),
    bDmu[sex] ~ normal(0,0.5),
    bHmu[sex] ~ normal(0,0.5),
    bBMImu[sex] ~ normal(0,0.5),
    
    sigma_nat ~ exponential(1),
    sigma ~ exponential(1),
    
    Rho ~ lkj_corr(2)
  ), data=d.list2 , chains=10 , cores=10, log_lik=TRUE ,iter = 8000, control=list(max_treedepth=18,adapt_delta=0.992))
save(mBMI,file="mBMI.Rdata")

prio<-extract.prior(mBMI,n=10000)
save(prio,file="mBMI.prior.Rdata")

start-Sys.time()

#Model comparison
load("mBMI.Rdata")
load("m1.BMI.Rdata")

(comp.tab<-compare(m1.BMI,mBMI))
comp.tab<-cbind(model=row.names(comp.tab),round(comp.tab,2))
write.table(comp.tab,file="WAIC.table.txt",row.names=F,sep="\t")


