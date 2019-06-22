library(AER)

mydata <- read.table("fulton.data", sep= "\t", header=T)
attach(mydata)

#ols model in log
fit.ols <- lm(LogQuantity ~ LogPrice + Monday + Tuesday + Wednesday + Thursday + Cold + Rainy)
summary(fit.ols)

#IV & stormy as instrument

fit.iv <- ivreg(LogQuantity ~ LogPrice + Monday + Tuesday + Wednesday + Thursday + Cold + Rainy | Stormy
                  + Monday + Tuesday + Wednesday + Thursday + Cold + Rainy)

summary(fit.iv)

#IV & stormy - windspeed instrument

fit.iv <- ivreg(LogQuantity ~ LogPrice + Monday + Tuesday + Wednesday + Thursday + Cold + Rainy | Stormy
                  + Monday + Tuesday + Wednesday + Thursday + Cold + Rainy + Windspeed)

summary(fit.iv, diagnostic=T) #stat test for iv
