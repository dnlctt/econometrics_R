library(foreign)
library(xts)
library(ggplot2)
library(pastecs)

mydata <- read.dta("djia_daily.dta")
head(mydata)
tail(mydata)
View(mydata)
str(mydata)

mydata[2:length(mydata[,4]),4]
mydata[2:length(mydata[,4]),4] <- 100*diff(log(mydata[,4]))

rdjia <- xts(mydata[, 4], mydata[, 5])
View(mydata$djia)

plot(rdjia, main = "DJIA returns")
png(file = "plot_ret.png", bg = "transparent")

sd(mydata$djia, na.rm = TRUE)

rdjia_sub <- rdjia["19870901/19871017"]
plot(rdjia_sub, main = "DJIA returns")

png(file = "plot_subsample_return.png", bg = "transparent")
plot(rdjia_sub, main = "DJIA returns")


mean(rdjia_sub)
var(rdjia_sub)
sd(rdjia_sub)

d_ret <- density(rdjia_sub) # create the density  
plot(d_ret, main="Density") # density graph

png(file = "plot_density.png")
plot(d_ret, main="Density")

dgauss = rnorm(length(rdjia_sub), mean = 5, sd = 2) #generate random number

View(dgauss)
summary(dgauss)
sd(dgauss)

#Plot the scatter between the variables "dgauss" and "rdjia_sub"
plot(dgauss, rdjia_sub, main="Returns and dgauss", xlab="time ", ylab="ret ", pch=1)

png(file = "scatter_ret_gaus.png")
plot(dgauss, rdjia_sub, main="Returns and dgauss", xlab="time ", ylab="ret ", pch=1)
dev.off()
