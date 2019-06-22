library(plm)
library(car)
library(foreign)

dataset <- read.csv("liquor.txt", sep=" ", header=T)
View(dataset)
attach(dataset)

liq <- pdata.frame(dataset, index = c("hh", "time"))
View(liq)

#pooled ols with lm
pooled.ols <-lm(l ~ x, data=liq)
summary(pooled.ols)
          
           #with plm
pooled.ols.2 <- plm(l~x, model="pooling", data = liq) 
summary(pooled.ols.2, robust = FALSE)

plot(liq$x, liq $l, pch=19, xlab="income", ylab="expenditure on liquor")
abline(lm(liq$l ~ liq$x),lwd=3, col="red")

#fixed effect FEmodel
fixed.effects <- plm(l~x, model="within", data = liq)
summary(fixed.effects, robust = FALSE)

#test btw OLS or FE model
pFtest(fixed.effects, pooled.ols)

#random effect 
random.effects <- plm(l~x, model="random", data = liq)
summary(random.effects, robust = FALSE)

#fe vs re - Hausman test
phtest(fixed.effects, random.effects)

