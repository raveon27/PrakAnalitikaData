#Pengaturan direktori kerja
setwd("D:/SEM VII/ASPRAK ANALITIKA DATA/Data & R Script P2")

#  Plot fungsi distribusi logistik
x = seq(-5, 5, length= 1000)
pi = 3.1415926
logit = 1/(1+exp(-x*pi/sqrt(2)))

plot(x,logit,type="l", ylab="Distribution Function", main="Logit Distribution Function",cex=1.2)

#import data Fullcoverage.csv 
library(readr)
Fullcov <- read_csv("FullCoverage.csv")
head(Fullcov)
unique(Fullcov$marital)

#  Tabel deskriptif statistik
table(Fullcov$y)

tab22<-function(a,y){table(a,y)
  print(table(a,y))
  options(digits=4)
  print(prop.table(table(a,y),1)*100)
  print(margin.table(table(a,y),1))
  options(digits=7)}
tab22(Fullcov$men,Fullcov$y)
tab22(Fullcov$urban,Fullcov$y)
tab22(Fullcov$private,Fullcov$y)
tab22(Fullcov$marital,Fullcov$y)

by(Fullcov$age,list(Fullcov$y),FUN=mean) 
mean(Fullcov$age)

by(Fullcov$seniority,list(Fullcov$y),FUN=mean) 
mean(Fullcov$seniority)

##########
# Contoh 1
##########

library(Hmisc)
library(tidyverse)

#Penentuan Reference Category
Fullcov2=Fullcov%>%
  mutate(marital=ifelse(marital=="S",0,marital))

#Model Regresi Logistik
FullcovModel = glm(y~men+urban+private+factor(marital)+age+seniority,family=binomial(link=logit),data=Fullcov2)
summary(FullcovModel)
logLik(FullcovModel)

# ODDS-RATIO
exp(coef(summary(FullcovModel,1)))

# Tabel Klasifikasi
haty=predict(FullcovModel,Fullcov2, type ='response')
binhaty=(haty>0.34675)
table(Fullcov2$y,binhaty)

# Goodness of fit
# LIKELIHOOD RATIO TEST

# REDUCED LOGIT MODEL;
FullcovModelRed = glm(y~1, family=binomial(link=logit),data=Fullcov2)
logLik(FullcovModelRed)
anova(FullcovModelRed,FullcovModel, test="Chisq")
2*(logLik(FullcovModel)-logLik(FullcovModelRed))

# PSEUDO R-2
1-(logLik(FullcovModel))/(logLik(FullcovModelRed))

# Hosmer and Lemeshow Test
library(ResourceSelection)
h1<-hoslem.test(FullcovModel$y,fitted(FullcovModel),g=10)
h1

##########
# Contoh 2
##########

Fullcov<- read_csv("TypeofCoverage.csv")
unique(Fullcov$yord)

# ORDERED LOGISTIC REGRESSION
library(rms)
ddist<- datadist(Fullcov$men,Fullcov$urban,Fullcov$private,Fullcov$marital_M,Fullcov$marital_O,Fullcov$age,Fullcov$seniority)
options(datadist='ddist')
ologit<- lrm(yord ~ men+urban+private+marital_M+marital_O+age+seniority, data=Fullcov, na.action=na.pass)
print(ologit)
options(digits=3)
exp(ologit$coefficients)

library(MASS)
ologit2<-polr(as.factor(yord) ~ men+urban+private+marital_M+marital_O+age+seniority, data=Fullcov, na.action=na.pass,Hess=T)
summary(ologit2)
#########
# Contoh 3
#########

Vehown <- read_csv("VehOwned.csv")
unique(Vehown$veh)

library(mlogit)
# PREPARE THE DATA
Vehown$veh<-as.factor(Vehown$veh)
mldata<-mlogit.data(Vehown, varying=NULL, choice="veh", shape="wide")

# GENERALIZED LOGIT MODEL
glogit.model<- mlogit(veh~1|men+age+urban, data = mldata, reflevel="C")
summary(glogit.model)

#########
# Contoh 4
#########

VehownPrice <- read_csv("VehChoicePrice.csv")

# PREPARE THE DATA
mldata2<-mlogit.data(VehownPrice, varying=5:7, choice="veh", shape="wide")
head(mldata2,5)


# MULTINOMIAL LOGISTIC REGRESSION
mlogit.model<- mlogit(veh~price|men+urban+age, data = mldata2, reflevel="C")
summary(mlogit.model)


