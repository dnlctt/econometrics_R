library(foreign)
library(xts)
library(zoo)
library(lmtest)

food <- read.csv("food.data", sep=" ", header=T)
attach(food)

#ols model
model.ols <- lm(food_exp ~ income)
summary(model.ols)

#breusch-pagan test
bptest(model.ols, ~ income, studentize = F)

#white_test
u2 <- summary(model.ols)$residuals^2
R2 <- summary(lm(u2 ~ income + I(income^2)))$r.squared #auxiliary regression

LMstats <- nrow(food)*R2
LMstats

p.value.ols <- pchisq(LMstats, df, lower.tail = F)
p.value.ols

#white test as extreme case of bptest
bptest(model.ols, ~ income + I(income^2))

#weighted least squared model
nyw = food_exp/sqrt(income)
nxw = income/sqrt(income)
ncw = 1/sqrt(income)

fit.w <- lm(nyw ~ nxw + ncw -1)

summary(fit.w)
