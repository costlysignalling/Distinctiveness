#Load the data
d<-read.table("data.clean.txt",sep="\t",header=T,stringsAsFactors = F)
d$BMI<-d$weight/((d$height/100)^2)

#set factors with proper level orderings
d$nat<-factor(d$nat,levels = c("NAM","CMR","TR","RO","CZ","UK","COL","BRAZ","IND","VIE"))
d$sex<-factor(d$sex,levels = c("F","M"))

#composite factor variable
d$stan.lab<-factor(paste(d$sex,d$nat),levels=paste(rep(c("F","M"),times=8),rep(c("NAM","CMR","TR","RO","CZ","UK","COL","BRAZ","IND","VIE"),each=2)))

#here you can remove the observations where height or age is lacking
# d<-d[!is.na(d$BMI),]
# nrow(d)
# d<-d[!is.na(d$age),]
# nrow(d)

#Row labels for summarization tabs
labtab<-c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness","Height","BMI","Age")

meantab<-rbind(
  tapply(d$Attractiveness,d$stan.lab,mean,na.rm=T),
  tapply(d$Sextypicality,d$stan.lab,mean,na.rm=T),
  tapply(d$Asymmetry,d$stan.lab,mean,na.rm=T),
  tapply(d$Distinctiveness,d$stan.lab,mean,na.rm=T),
  tapply(d$height,d$stan.lab,mean,na.rm=T),
  tapply(d$BMI,d$stan.lab,mean,na.rm=T),
  tapply(d$age,d$stan.lab,mean,na.rm=T))

sdtab<-rbind(
  tapply(d$Attractiveness,d$stan.lab,sd,na.rm=T),
  tapply(d$Sextypicality,d$stan.lab,sd,na.rm=T),
  tapply(d$Asymmetry,d$stan.lab,sd,na.rm=T),
  tapply(d$Distinctiveness,d$stan.lab,sd,na.rm=T),
  tapply(d$height,d$stan.lab,sd,na.rm=T),
  tapply(d$BMI,d$stan.lab,sd,na.rm=T),
  tapply(d$age,d$stan.lab,sd,na.rm=T))

rownames(meantab)<-labtab
rownames(sdtab)<-labtab

meantab
sdtab

#Indian sample lacks information about body height and weight
write.table(meantab,"meantab.txt",col.names=NA,sep="\t")
write.table(sdtab,"sdtab.txt",col.names=NA,sep="\t")

restab<-as.data.frame(t(matrix(paste(format(round(meantab,2),nsmall=2)," (",format(round(sdtab,2),nsmall=2),")",sep=""),ncol=ncol(meantab))))
restab<-cbind(nat=sapply(strsplit(colnames(meantab)," "),function(x)x[2]),
              sex=sapply(strsplit(colnames(meantab)," "),function(x)x[1]),
              restab)

names(restab)[3:9]<-rownames(meantab)
write.table(restab,"restab.txt",row.names=F,sep="\t")

names(d)[c(1:6,15)]

d.s<-data.frame(
  d[,c(1:6,15)],
  Attractiveness=scale(d$Attractiveness),
  Sextypicality=scale(d$Sextypicality),
  Asymmetry=scale(d$Asymmetry),
  Distinctiveness=scale(d$Distinctiveness),
  Height=scale(d$height),
  BMI=scale(d$BMI),
  Age=scale(d$age))

chars<-c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness","height","BMI","Age")

ALLmeans<-c(mean(d$Attractiveness,na.rm=T),
            mean(d$Sextypicality,na.rm=T),
            mean(d$Asymmetry,na.rm=T),
            mean(d$Distinctiveness,na.rm=T),
            mean(d$height,na.rm=T),
            mean(d$BMI,na.rm=T),
            mean(d$age,na.rm=T))

ALLsds <- c(sd(d$Attractiveness,na.rm=T),
            sd(d$Sextypicality,na.rm=T),
            sd(d$Asymmetry,na.rm=T),
            sd(d$Distinctiveness,na.rm=T),
            sd(d$height,na.rm=T),
            sd(d$BMI,na.rm=T),
            sd(d$age,na.rm=T))

names(ALLmeans)<-chars
names(ALLsds)<-chars

#Personalized scale function
scaleP<-function(x,char,m=ALLmeans,s=ALLsds){(x-m[which(names(m)==char)])/s[which(names(s)==char)]}
scale(d$Attractiveness)==scaleP(d$Attractiveness,"Attractiveness")

#Create a function that allows to easily reconstruct the original values (useful for predictions on the original scale)
descaleP<-function(x,char,m=ALLmeans,s=ALLsds){x*s[which(names(s)==char)]+m[which(names(m)==char)]}

descaleP(scale(d$Attractiveness),"Attractiveness")==d$Attractiveness

#Check how means and SDs look by nation
tapply(d.s$Attractiveness,d$nat,mean,na.rm=T)
tapply(d.s$Sextypicality,d$nat,mean,na.rm=T)
tapply(d.s$Asymmetry,d$nat,mean,na.rm=T)
tapply(d.s$Distinctiveness,d$nat,mean,na.rm=T)
tapply(d.s$Height,d$nat,mean,na.rm=T)
tapply(d.s$BMI,d$nat,mean,na.rm=T)
tapply(d.s$Age,d$nat,mean,na.rm=T)


tapply(d.s$Attractiveness,d$nat,sd,na.rm=T)
tapply(d.s$Sextypicality,d$nat,sd,na.rm=T)
tapply(d.s$Asymmetry,d$nat,sd,na.rm=T)
tapply(d.s$Distinctiveness,d$nat,sd,na.rm=T)
tapply(d.s$Height,d$nat,sd,na.rm=T)
tapply(d.s$BMI,d$nat,sd,na.rm=T)
tapply(d.s$Age,d$nat,sd,na.rm=T)


#See by sample counts and level orderings
table(as.integer(d.s$nat),d$nat)
table(as.integer(d.s$sex),d$sex)
table(as.integer(d.s$stan.lab),d$stan.lab)


