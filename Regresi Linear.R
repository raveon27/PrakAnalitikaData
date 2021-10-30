data=read.delim('clipboard')
head(data)
summary(data)

model.1<-lm(TARGET_deathRate~incidenceRate + popEst2015 + povertyPercent +
		studyPerCap + MedianAge + AvgHouseholdSize + PctPublicCoverage + 
		PctWhite + PctBlack,data=data)
summary(model.1)

#Uji Normalitas
shapiro.test(data$TARGET_deathRate)

#Matriks Korelasi
cor(data)

#Scatter plot
plot(data$incidenceRate,data$TARGET_deathRate,main="Scatter Plot",
	xlab="incidenceRate",ylab="TARGET_deathRate",pch=19)
abline(lm(TARGET_deathRate~incidenceRate,data=data),col="Blue")
library(car)
scatterplot(TARGET_deathRate~incidenceRate,data=data)

#Scatter matrix
pairs(data,pch=19)
install.packages('psych')
library(psych)
pairs.panels(data,method="pearson")

#diagnostic
#nilai vif
vif(model.1)
#durbin watson
library(lmtest)
dwtest(model.1)

#criterion
AIC(model.1)
BIC(model.1)
PRESS<-function(linear.model){
	pr<-residuals(linear.model)/(1-lm.influence(linear.model)$hat)
	sum(pr^2)
}
PRESS(model.1)

#residual plot
plot(fitted(model.1),resid(model.1),xlab="Fitted Value",ylab="Std Residual",main="Residual Plot")
abline(0,0)

#QQ Plot
qqnorm(resid(model.1))
qqline(resid(model.1))
