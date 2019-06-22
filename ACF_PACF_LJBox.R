library(zoo)
library(lmtest)
library(dynlm)
library(orcutt)
library(sandwich)
library(pastecs)

mydata = read.table("bnews.data", header = T, sep = "\t", dec = ".")

attach(mydata)
mrk <- ts(Market.capitalization)
oilp <- ts(Oil.price.above.benchmark.price)

#ols model correlation mkt & oil
fit <- lm(mrk ~ oilp)
summary(fit)

#dynamic lm model lag 1
fit1 <- dynlm(mrk ~ oilp + L(oilp, 1))
summary(fit1)

bptest(fit1, studentize = F) #check heteroskedasticity


bgtest(fit1, order = 6, type = c("F")) #check autocorrelation lmf stats
bgtest(fit1, order = 6, type = c("Chisq")) #check autocor chisq stats

#LjunBox for autocor
Box.test(fit1$residuals, type='Ljung-Box', lag=6)

#Model lag 4
fit2 <- dynlm(mrk ~ oilp + L(oilp, 1) + L(oilp, 2)
                + L(oilp, 3) + L(oilp, 4))
summary(fit2)


acf(fit2$residuals, lag.max = 6) #autocor funct
acf(fit2$residuals, type = "partial" , lag.max = 6) #pacf
cochrane.orcutt(fit2, convergence = 8, max.iter=100) #HO test
