#Data NMINER
install.packages("GLMsData")
library(GLMsData)
data(nminer)

#Pembentukan Model GLM
nm.m1<-glm(Minerab~Eucs, data=nminer, family=poisson) 
summary(nm.m1)

nm.m0<-glm(Minerab~1, data=nminer, family=poisson) #Model yang hanya melibatan konstan
nm.m1<-glm(Minerab~Eucs, data=nminer, family=poisson)#Model dengan satu prediktor

#Uji Rasio Likeliood
L<-deviance(nm.m0)-deviance(nm.m1)
L 

pchisq(L, df.residual(nm.m0) - df.residual(nm.m1), lower.tail=FALSE) 

#Analisis Deviansi
anova(nm.m1, test="Chisq")