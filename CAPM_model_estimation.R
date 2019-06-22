library(foreign)
library(xts)
library(ggplot2)

mydata <- read.csv("capm.data", sep=" ", header=T)

par(mfrow=c(2,2))

plot(mydata$Excessreturncompanya, mydata$excessreturnmarketportfolio,
       + main="Excess return on shares and excess return on stock index",
       +  col.main="blue", font.main="4", xlab="ret", ylab="ret")
plot(mydata$Excessreturncompanya, mydata$sales000,
       + main="Excess return on shares and sales",
       + col.main="blue", font.main="4",xlab="ret", ylab="sales")
plot(mydata$Excessreturncompanya, mydata$debt000,
       + main="Excess return on shares and debt",
       + col.main="blue", font.main="4", xlab="ret", ylab="debt")

fit <- lm(mydata$Excessreturncompanya ~ mydata$excessreturnmarketportfolio
            + mydata$sales000 + mydata$debt000)
summary(fit)
