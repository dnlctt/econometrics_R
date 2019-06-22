library(stargazer)
library(car)

hprice <- read.csv("hprice.data", sep=" ",head=T)

summary(hprice)

par(mfrow=c(2,2))

plot(hprice$saleprice, hprice$lotsize , main="lotsize", col.main="blue", font.main="4")
plot(hprice$saleprice, hprice$bedroom, main="bedroom", col.main="blue", font.main="4")
plot(hprice$saleprice, hprice$bath, main="bath", col.main="blue", font.main="4")
plot(hprice$saleprice, hprice$stories, main="stories", col.main="blue", font.main="4")

#model
fit.3.1 = (lm(hprice$saleprice ~ hprice$lotsize +hprice$bedroom + hprice$bath + hprice$stories))
summary(fit.3.1)

correlation = cor(hprice$saleprice, hprice$lotsize,
                    + hprice$bedroom, hprice$bath, hprice$bath)