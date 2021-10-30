#Ilustrasi Empiris
#Pembentukan level kategori
library(tidyverse)
sg_auto2<-sg_auto%>%
  mutate(NCDClass=ifelse(NCD==0,"c1",ifelse(NCD==10 | NCD==30,"c2","c0")),
         VAgecat1=ifelse(VAgecat1<=2,"c1",ifelse(VAgecat1<=5,"c2",ifelse(VAgecat1<=10,"c3",ifelse(VAgecat1<=15,"c4","c0")))))

#Regresi Poisson
m_pois<-glm(Clm_Count~as.factor(NCDClass)+as.factor(VAgecat1),data=sg_auto2,family=poisson)
summary(m_pois)

#Studi Kasus
library(AER)
library(pscl)
library(MASS)

data("NMES1988")
head(NMES1988)
dt <- NMES1988[, c(1, 6:8, 13, 15, 18)]
head(dt)

hist(dt$visits, breaks = 0:90 - 0.5,main="Distribusi Frekuensi")

#Regresi Poisson
fm_pois<-glm(visits~.,data=dt,family=poisson)
summary(fm_pois)

coeftest(fm_pois, vcov = sandwich)

#Regresi Negatif Binomial
fm_nbin <- MASS::glm.nb(visits ~ ., data = dt)
summary(fm_nbin)

#Regresi Zero-Inflated
fm_zinb0 <- zeroinfl(visits ~ ., data = dt, dist = "negbin")
summary(fm_zinb0)

#Pembandingan Model
fm<-list("ML-Pois" = fm_pois,"NB" = fm_nbin, "ZINB" = fm_zinb0)
sapply(fm, function(x) coef(x)[1:8])

cbind("ML-Pois" = sqrt(diag(vcov(fm_pois))),"Adj-Pois" = sqrt(diag(sandwich(fm_pois))),sapply(fm[-1], function(x) sqrt(diag(vcov(x)))[1:8]))

rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),Df = sapply(fm, function(x) attr(logLik(x), "df")))

round(c("Obs" = sum(dt$visits < 1),
        "ML-Pois" = sum(dpois(0, fitted(fm_pois))),
        "NB" = sum(dnbinom(0, mu = fitted(fm_nbin), size = fm_nbin$theta)),
        "ZINB" = sum(predict(fm_zinb0, type = "prob")[,1])))
