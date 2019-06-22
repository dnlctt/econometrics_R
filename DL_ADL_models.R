library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
library(car)
library(nlWaldTest)
library(forecast)
library(xts)
library(stats4)

dataset <- read.table("okun.txt", sep= "\t", header=T)
attach(dataset)

str(dataset)
dataset <- ts(dataset, start = c(1985, 2), end = c(2009,3), frequency = 4)
u <- ts(dataset[,3], start = c(1985, 2), end = c(2009,3), frequency = 4)
g <- ts(dataset[,2], start = c(1985, 2), end = c(2009,3), frequency = 4)
plot(u,type="l",col="red")
plot(g,type="l",col="blue")

#model DL 
#q = 6
u <- window(u,start=c(1985,2), end = c(2009,3))
g <- window(g,start=c(1985,2), end = c(2009,3))

fit6 <- dynlm(d(u) ~ g + L(g, 1) + L(g, 2)+ L(g, 3) + L(g, 4) + L(g, 5) + L(g, 6))
summary(fit6)

#q = 5
u <- window(u,start=c(1985,3))
g <- window(g,start=c(1985,3))
fit5 <- dynlm(d(u) ~ g + L(g, 1) + L(g, 2) + L(g, 3) + L(g, 4) + L(g, 5))
summary(fit5)

#q=4
u <- window(u,start=c(1985,4), end = c(2009,3))
g <- window(g,start=c(1985,4), end = c(2009,3))
fit4 <- dynlm(d(u) ~ g + L(g, 1) + L(g, 2) + L(g, 3)+ L(g, 4))
summary(fit4)

#comparing BIC-s
BIC(fit6)
BIC(fit5)
BIC(fit4)

#model ARDL(2,2)
u <- ts(dataset[,3], start = c(1985, 2), end = c(2009,3), frequency = 4)
g <- ts(dataset[,2], start = c(1985, 2), end = c(2009,3), frequency = 4)

fitardl <- dynlm(d(u) ~ g + L(g, 1) + L(g, 2) + L(d(u),1) + L(d(u),2))
summary(fitardl)
