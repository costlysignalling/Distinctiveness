library(rethinking)

source("01_data_by_rating_prepare.R")

names(dl)

summary(as.factor(dl$rating))

nrow(dl)
dl<-dl[!is.na(match(dl$rating,1:7)),]
nrow(dl)

#Some extra rubbish summary to add to sample size table in supplement, go to line 39 where the interesting analysis starts
summary(dl$nat)
sum(summary(dl$nat))
mean(summary(dl$nat))
sd(summary(dl$nat))

unique(dl$nat)
sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))})
sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x & dl$sex=="F"]))})
sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x & dl$sex=="M"]))})

sum(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))}))
mean(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))}))
sd(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))}))

sum(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="F"]))}))
mean(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="F"]))}))
sd(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="F"]))}))

sum(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="M"]))}))
mean(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="M"]))}))
sd(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="M"]))}))

sapply(unique(dl$nat), function(x){length(unique(dl$rater[dl$nat==x & dl$sex=="F"]))})

#Here starts interesting analysis
dl$nat<-factor(dl$nat,levels = c("NAM","CMR","TR","RO","CZ","UK","BRAZ","VIE"))

d.list<-list(rating=as.integer(as.numeric(dl$rating)),
             tar=as.integer(as.factor(paste(dl$nat,dl$target))),
             rat=as.integer(as.factor(paste(dl$nat,dl$rater))),
             Sextypicality=dl$Sextypicality,
             Asymmetry=dl$Asymmetry,
             Distinctiveness=dl$Distinctiveness,
             sex=as.integer(as.factor(dl$sex)),
             nat=as.integer(as.factor(dl$nat)))

str(d.list)

range(d.list$nat)

m1<-ulam(
  alist(
    rating ~ ordered_logistic(phi,alpha),
    
    logit(phi) <- pars[nat,(sex-1)*4+1]+pars[nat,(sex-1)*4+2]*Sextypicality+pars[nat,(sex-1)*4+3]*Asymmetry+pars[nat,(sex-1)*4+4]*Distinctiveness + pr[rat,1] + pr[rat,2]*Sextypicality + pr[rat,3]*Asymmetry + pr[rat,4]*Distinctiveness + z_t[tar]*sigma_t,
    
    vector[8]:pars[nat] ~ multi_normal(c(amu[1],bSTmu[1],bAsmu[1],bDmu[1],amu[2],bSTmu[2],bAsmu[2],bDmu[2]),Rho,sigma_nat),
    vector[4]:pr[rat] ~ multi_normal(c(0,0,0,0),Rho_r,sigma_r),
    z_t[tar] ~ normal(0,1),
    
    amu[sex] ~ normal(0,2.5),
    bSTmu[sex] ~ normal(0,2.5),
    bAsmu[sex] ~ normal(0,2.5),
    bDmu[sex] ~ normal(0,2.5),
    
    sigma_nat ~ exponential(1),
    sigma_r ~ exponential(1),
    sigma_t ~ exponential(1),

    Rho ~ lkj_corr(2),
    Rho_r ~ lkj_corr(2),
    alpha ~ normal(0,1.5)
    
  ), data=d.list , chains=4 , cores=4, log_lik=TRUE ,iter = 750, warmup=250, sample=T)

stancode(m1)

#, control=list(max_treedepth=18,adapt_delta=0.99)
save(m1,file="m1_by_rating.Rdata")

prio<-extract.prior(m1,n=1000)
save(prio,file="m1_by_rating.prior.Rdata")

