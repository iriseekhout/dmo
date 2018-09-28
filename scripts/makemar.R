#make missings

load("data/costdata_complete.rda")

x <- costdata_complete

alpha <- c(0.17,0.35,0.50)

# kosten; CostsTotaal en voor de effecten; CSoverig, Neonataladmission, en Haemorrhage1000YN
#meer in kosten dan in effecten (2:1)
#patterns
p0 <- rep(1,ncol(x))
p7 <- p6 <- p5 <- p4 <- p3 <- p2 <- p1 <-p0
p1[which(names(x)=="CostsTotaal")]<-0#alleen kosten
p2[which(names(x)=="CostsTotaal"| names(x)=="CSoverig")]<-0#zowel kosten als deel effecten
p3[which(names(x)=="Neonataladmission"|names(x)=="CSoverig")]<-0 #alleen effecten
p4[which(names(x)=="CostsTotaal"|names(x)=="haemorrhage1000YN")]<-0#zowel kosten als deel effecten
p5[which(names(x)=="CostsTotaal"|names(x)=="haemorrhage1000YN"|names(x)=="Neonataladmission")]<-0 #zowel kosten als deel effecten
p6[which(names(x)=="CSoverig"|names(x)=="haemorrhage1000YN"|names(x)=="Neonataladmission")]<-0 #alleen effecten
p7[which(names(x)=="CostsTotaal"|names(x)=="CSoverig"|names(x)=="haemorrhage1000YN"|names(x)=="Neonataladmission")]<-0 #alle kosten & effecten

pattern <- rbind(p1,p2,p3,p4,p5,p6,p7)

f <- c(0.4,0.10,0.05,0.10,0.15,0.05,0.15)

#-	Gravidity
#-	Parity
#-	Height
#-	Weight
#-	Hypertensive medication (HypertensiveYN)
#-	Diabetes medication  (DiabetesYN)
#-	Elective medication (ElectiveYN)
#-	Other medication (OtherYN)
#-	Age
#-	BMI
#-	Gestational age
# -	Treatmentgroup
a <- pattern*0
a[,which(names(x)=="Gravidity"|names(x)=="Parity"|names(x)=="Height"|names(x)=="Weightmother"|names(x)=="hypertensiveYN"|
         names(x)=="diabetesYN"|names(x)=="electiveYN"|names(x)=="OtherYN"|names(x)=="AgeMother"|names(x)=="BMI"|
         names(x)=="GestationalAge"|names(x)=="Treatmentgroup")] <- 1
g <- rep(4,nrow(pattern))
quant <- matrix(rep(0.5,nrow(pattern)))

set.seed(321654)
miss <- MAR(x=x,alpha=alpha[1], pattern=pattern, f=f,a=a,g=g,quant=quant)

#aantal missings in kosten en effecten
sum(is.na(miss$xobs[,22]))
sum(is.na(miss$xobs[,15]))
sum(is.na(miss$xobs[,23]))
sum(is.na(miss$xobs[,16]))

dfout <- x
dfout[is.na(miss$xobs)]<-NA


sum(is.na(dfout[,22]))
sum(is.na(dfout[,15]))
sum(is.na(dfout[,23]))
sum(is.na(dfout[,16]))

1-nrow(na.omit(dfout))/nrow(dfout)

costdata_missing17 <- dfout

save("costdata_missing17", file = "data/costdata_missing17.rda",compress = "xz")

set.seed(987654)
miss <- MAR(x=x,alpha=alpha[2], pattern=pattern, f=f,a=a,g=g,quant=quant)

#aantal missings in kosten en effecten
sum(is.na(miss$xobs[,22]))
sum(is.na(miss$xobs[,15]))
sum(is.na(miss$xobs[,23]))
sum(is.na(miss$xobs[,16]))

dfout <- x
dfout[is.na(miss$xobs)]<-NA


sum(is.na(dfout[,22]))
sum(is.na(dfout[,15]))
sum(is.na(dfout[,23]))
sum(is.na(dfout[,16]))

1-nrow(na.omit(dfout))/nrow(dfout)

costdata_missing35 <- dfout

save("costdata_missing35", file = "data/costdata_missing35.rda",compress = "xz")


set.seed(654987)
miss <- MAR(x=x,alpha=alpha[3], pattern=pattern, f=f,a=a,g=g,quant=quant)

#aantal missings in kosten en effecten
sum(is.na(miss$xobs[,22]))
sum(is.na(miss$xobs[,15]))
sum(is.na(miss$xobs[,23]))
sum(is.na(miss$xobs[,16]))

dfout <- x
dfout[is.na(miss$xobs)]<-NA


sum(is.na(dfout[,22]))
sum(is.na(dfout[,15]))
sum(is.na(dfout[,23]))
sum(is.na(dfout[,16]))

1-nrow(na.omit(dfout))/nrow(dfout)

costdata_missing50 <- dfout

save("costdata_missing50", file = "data/costdata_missing50.rda",compress = "xz")
