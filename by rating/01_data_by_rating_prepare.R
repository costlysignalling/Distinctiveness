folder0<-getwd()

source("01_data_preparation.R")

setwd(file.path(folder0,"ind_ratings"))
list.files()

trlong<-function(set,nat="",tsex=""){
  rating=unlist(set[,2:ncol(set)])
  out<-data.frame(
    nat=rep(nat,length(rating)),
    sex=rep(tsex,length(rating)),
    target=rep(names(set)[2:ncol(set)],each=nrow(set)),
    rater=rep(c(set[,1]),times=ncol(set)-1),
    rating=rating)
  rownames(out)<-NULL
  return(out)
}

#Original levels c("NAM","CMR","TR","RO","CZ","UK","COL","BRAZ","IND","VIE")

nam1<-read.delim("Namibian_females_attr_rated_by_NAM_males_ED.txt")
nam2<-read.delim("Namibian_males_attr_rated_by_NAM_females_ED.txt")

(ln<-names(nam1)[2:ncol(nam1)])
(dn<-d$name[d$nat=="NAM" & d$sex=="F"])

ln<-sapply(ln,function(x){strsplit(x,"\\.")[[1]][3]})
dn<-sapply(dn,function(x){strsplit(strsplit(x,"'")[[1]][2],"\\.")[[1]][1]})

names(nam1)[2:ncol(nam1)]<-ln
d$name[d$nat=="NAM" & d$sex=="F"]<-dn

(ln<-names(nam2)[2:ncol(nam2)])
(dn<-d$name[d$nat=="NAM" & d$sex=="M"])

ln<-sapply(ln,function(x){strsplit(strsplit(x,"\\.")[[1]][1],"X")[[1]][2]})
dn<-sapply(dn,function(x){strsplit(strsplit(x,"'")[[1]][2],"\\.")[[1]][1]})

names(nam2)[2:ncol(nam2)]<-ln
d$name[d$nat=="NAM" & d$sex=="M"]<-dn

names(nam2)[2:ncol(nam2)][ln!=dn]<-"051MTji_ff" #t was capitalized only in one label

cmr1<-read.delim("Cameroon_2012_females_attr_rated_by_CMR_men_ED.txt")
cmr2<-read.delim("Cameroon_2012_males_attr_rated_by_CMR_females_ED.txt")
cmr3<-read.delim("Cameroon_2013_females_attr_rated_by_CMR_males_ED.txt")
cmr4<-read.delim("Cameroon_2013_males_attr_rated_by_CMR_females_ED.txt")
cmr5<-read.delim("Cameroon_2016_females_attr_rated_by_CMR_males_ED.txt")
cmr6<-read.delim("Cameroon_2016_males_attr_rated_by_CMR_females_ED.txt")

(ln<-names(cmr1)[2:ncol(cmr1)])
(dn<-d$name[d$nat=="CMR" & d$sex=="F" & d$set=="CMR12"])
sum(ln==dn)==length(ln)

(ln<-names(cmr2)[2:ncol(cmr2)])
(dn<-d$name[d$nat=="CMR" & d$sex=="M" & d$set=="CMR12"])
sum(ln==dn)==length(ln)

(ln<-names(cmr3)[2:ncol(cmr3)])
(dn<-d$name[d$nat=="CMR" & d$sex=="F" & d$set=="CMR13"])
sum(ln==dn)==length(ln)
ln[ln==dn]
ln[ln!=dn]
dn[ln!=dn]

dnat<-d$Attractiveness[match(dn,d$name)]
dlat<-colMeans(cmr3[,match(ln,names(cmr3))])

plot(dnat,dlat)
cor(dnat,dlat,use="complete.obs")

d$name[match(dn,d$name)]<-ln

(ln<-names(cmr4)[2:ncol(cmr4)])
(dn<-d$name[d$nat=="CMR" & d$sex=="M" & d$set=="CMR13"])
sum(ln==dn)==length(ln)
ln[ln==dn]
ln[ln!=dn]
dn[ln!=dn]

dn<-sapply(dn,function(x){strsplit(x,"\\.")[[1]][1]})
sum(ln==dn)==length(ln)
ln[ln==dn]
ln[ln!=dn]
dn[ln!=dn]
ln[ln!=dn]<-dn[ln!=dn]

names(cmr4)[2:ncol(cmr4)]<-ln
d$name[d$nat=="CMR" & d$sex=="M" & d$set=="CMR13"]<-dn

(ln<-names(cmr5)[2:ncol(cmr5)])
(dn<-d$name[d$nat=="CMR" & d$sex=="F" & d$set=="CMR16"])
sum(ln==dn)==length(ln)
ln[ln==dn]
ln[ln!=dn]
dn[ln!=dn]
dn<-paste("X",dn,sep="")
d$name[d$nat=="CMR" & d$sex=="F" & d$set=="CMR16"]<-dn

(ln<-names(cmr6)[2:ncol(cmr6)])
(dn<-d$name[d$nat=="CMR" & d$sex=="M" & d$set=="CMR16"])
dn<-paste("X",dn,sep="")
d$name[d$nat=="CMR" & d$sex=="M" & d$set=="CMR16"]<-dn

tr1<-read.delim("Turkey_BFD_females_attr_rated_by_TUR_bothsex_ED.txt")
tr2<-read.delim("Turkey_BFD_males_attr_rated_by_TUR_bothsex_ED.txt")

(ln<-names(tr1)[2:ncol(tr1)])
(dn<-d$name[d$nat=="TR" & d$sex=="F"])
dn<-paste("F",dn,"ATTR",sep="_")
sum(ln==dn)==length(ln)
d$name[d$nat=="TR" & d$sex=="F"]<-dn

(ln<-names(tr2)[2:ncol(tr2)])
(dn<-d$name[d$nat=="TR" & d$sex=="M"])
dn<-paste("M",dn,"ATTR",sep="_")
sum(ln==dn)==length(ln)
d$name[d$nat=="TR" & d$sex=="M"]<-dn

ro1<-read.delim("Romanian_females_attr_rated_by_ROM_males_ED.txt")
ro2<-read.delim("Romanian_males_attr_rated_by_ROM_females_ED.txt")

cz1<-read.delim("Czech_120_females_attr_rated_by_CZ_males_ED.txt")
cz2<-read.delim("Czech_120_males_attr_rated_by_CZ_females_ED.txt")
cz3<-read.delim("Czech_2016_females_attr_rated_by_CZ_males_ED.txt")
cz4<-read.delim("Czech_2016_males_attr_rated_by_CZ_females_ED.txt")

(ln<-names(cz1)[2:ncol(cz1)])
(dn<-d$name[d$nat=="CZ" & d$sex=="F" & d$set=="CZ120"])
sum(ln==dn)==length(ln)

(ln<-names(cz2)[2:ncol(cz2)])
(dn<-d$name[d$nat=="CZ" & d$sex=="M" & d$set=="CZ120"])
sum(ln==dn)==length(ln)

(ln<-names(cz3)[2:ncol(cz3)])
(dn<-d$name[d$nat=="CZ" & d$sex=="F" & d$set=="CZ16"])
dn<-paste("X",dn,sep="")
sum(ln==dn)==length(ln)
d$name[d$nat=="CZ" & d$sex=="F" & d$set=="CZ16"]<-dn

(ln<-names(cz4)[2:ncol(cz4)])
(dn<-d$name[d$nat=="CZ" & d$sex=="M" & d$set=="CZ16"])
dn<-paste("X",dn,sep="")
sum(ln==dn)==length(ln)
d$name[d$nat=="CZ" & d$sex=="M" & d$set=="CZ16"]<-dn

uk1<-read.delim("UK_females_attr_rated_by_UK_bothsex_ED.txt")
uk2<-read.delim("UK_males_attr_rated_by_UK_bothsex_ED.txt")

(ln<-names(uk1)[2:ncol(uk1)])
(dn<-d$name[d$nat=="UK" & d$sex=="F"])
dn<-ln
d$name[d$nat=="UK" & d$sex=="F"]<-dn

(ln<-names(uk2)[2:ncol(uk2)])
(dn<-d$name[d$nat=="UK" & d$sex=="M"])
dn<-ln
d$name[d$nat=="UK" & d$sex=="M"]<-dn

braz1<-read.delim("Brazilian_females_attr_rated_by_braz_males_ED.txt")
braz2<-read.delim("Brazilian_males_attr_rated_by_braz_females_ED.txt")

vie1<-read.delim("Vietnam_females_attr_rated_by_VT_males_ED.txt")
vie2<-read.delim("Vietnam_males_attr_rated_by_VT_females_ED.txt")

(ln<-names(vie1)[2:ncol(vie1)])
(dn<-d$name[d$nat=="VIE" & d$sex=="F"])
dn<-paste("X",dn,sep="")
d$name[d$nat=="VIE" & d$sex=="F"]<-dn

(ln<-names(vie2)[2:ncol(vie2)])
(dn<-d$name[d$nat=="VIE" & d$sex=="M"])
dn<-paste("X",dn,sep="")
d$name[d$nat=="VIE" & d$sex=="M"]<-dn

dl<-rbind(
  trlong(nam1,nat="NAM",tsex="F"),
  trlong(nam2,nat="NAM",tsex="M"),
  trlong(cmr1,nat="CMR",tsex="F"),
  trlong(cmr2,nat="CMR",tsex="M"),
  trlong(cmr3,nat="CMR",tsex="F"),
  trlong(cmr4,nat="CMR",tsex="M"),
  trlong(cmr5,nat="CMR",tsex="F"),
  trlong(cmr6,nat="CMR",tsex="M"),
  trlong(tr1,nat="TR",tsex="F"),
  trlong(tr2,nat="TR",tsex="M"),
  trlong(ro1,nat="RO",tsex="F"),
  trlong(ro2,nat="RO",tsex="M"),
  trlong(cz1,nat="CZ",tsex="F"),
  trlong(cz2,nat="CZ",tsex="M"),
  trlong(cz3,nat="CZ",tsex="F"),
  trlong(cz4,nat="CZ",tsex="M"),
  trlong(uk1,nat="UK",tsex="F"),
  trlong(uk2,nat="UK",tsex="M"),
  trlong(braz1,nat="BRAZ",tsex="F"),
  trlong(braz2,nat="BRAZ",tsex="M"),
  trlong(vie1,nat="VIE",tsex="F"),
  trlong(vie2,nat="VIE",tsex="M"))

dl$Sextypicality<-d.s$Sextypicality[match(paste(dl$nat,dl$sex,dl$target),
                                          paste(d$nat,d$sex,d$name))]

dl$Asymmetry<-d.s$Asymmetry[match(paste(dl$nat,dl$sex,dl$target),
                                  paste(d$nat,d$sex,d$name))]

dl$Distinctiveness<-d.s$Distinctiveness[match(paste(dl$nat,dl$sex,dl$target),
                                              paste(d$nat,d$sex,d$name))]

summary(as.factor(dl$nat[!is.na(dl$Sextypicality)]))
summary(as.factor(dl$nat[is.na(dl$Sextypicality)]))

setwd(folder0)
