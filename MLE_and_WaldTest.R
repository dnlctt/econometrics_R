library(sandwich)
library(het.test)
library(urca)
library(zoo)
library(lmtest)
library(vars)
library(bbmle)

mydata <- read.table("capm.data",header=TRUE,sep=" ",dec=".")
attach(mydata)

LL2 = function(b0, b1, b2, b3, sigma) { R = Excessreturncompanya - b0 - b1*excessreturnmarketportfolio - b2*sales000 - b3*debt000
                                       R = suppressWarnings(dnorm(R, 0, sigma))-sum(log(R))}

mle2.model <- mle2(LL2,start=list(b0=0, b1=0, b2=0, b3=0, sigma=1),use.ginv=FALSE)

summary(mle2.model)

logLik(mle2.model)

confint(mle.model) #confint

#restricted model

LL2.restr = function(b0, b1, sigma) {
  + R = Excessreturncompanya - b0 - b1*excessreturnmarketportfolio
  + R = suppressWarnings(dnorm(R, 0, sigma))
  + -sum(log(R))}

mle2.model.restr <- mle2(LL2.restr,start=list(b0=0, b1=0, sigma=1),use.ginv=FALSE)
summary(mle2.model.restr)

lrtest(mle2.model, mle2.model.restr) #likelihood ratio test
anova(mle2.model, mle2.model.restr)

#Wald Test
fit1 <- lm(Excessreturncompanya ~ excessreturnmarketportfolio + sales000 + debt000)
rssq <- sum(resid(fit1)^2)
rfit1 <- lm(Excessreturncompanya~excessreturnmarketportfolio)
rrssq <- sum(resid(rfit1)^2)
wald <- (rrssq-rssq)/(rssq/nobs(fit1))
