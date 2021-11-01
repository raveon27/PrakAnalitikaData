##Simulate Linear Model
library(car)
library(MASS)
library(lmtest)
library(tseries)
library(ggfortify)

set.seed(1234)

##Linear Regression
#Generate the independent variable and the error
x1=rnorm(100,50,9)
x2=rnorm(100,200,64)
error=rnorm(100,0,16)
#Generate the dependent variable (b0=150, b1=-4, b2=2.5)
y1=150-(4*x1)+(2.5*x2)+error
#create the model
m1=lm(y1~x1+x2)
summary(m1)
autoplot(m1)

##Linear Regression with binary categorical independent variable
#Generate the independent variable and the error
x1=rnorm(100,50,9)
x2=rnorm(100,200,64)
x3=rbinom(100,1,0.7)
error=rnorm(100,0,16)
#Generate the dependent variable (b0=150, b1=-4, b2=2.5, b3=5)
y2=150-(4*x1)+(2.5*x2)+(5*x3)+error
#create the model
m2=lm(y2~x1+x2+x3)
summary(m2)
autoplot(m2)

##Poisson Regression
#Generate the independent variable
x1=rnorm(100,2,1)
x2=rnorm(100,1,1)
log.mean=5-(4*x1)+(2.5*x2)
#Generate the dependent variable
y3=rpois(100, exp(log.mean))
#create the model
m3=glm(y3~x1+x2,family=poisson(link='log'))
summary(m3)
autoplot(m3)
