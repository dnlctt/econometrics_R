#CAPM 
library(BatchGetSymbols)
library(rvest)
library(xml2)
library(quantmod)
library(foreign)
library(xts)
library(ggplot2)

l.out <- BatchGetSymbols(tickers, first.date, last.date, cache.folder)

first.date <- Sys.Date() - 60 

first.date <- "2018-12-21"
last.date <- Sys.Date() 

tickers <- c('RACE')

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         cache.folder = file.path(tempdir(),'BGS_Cache'))
View(l.out$df.control)

p <- xts(l.out$df.tickers$price.close, l.out$df.tickers$ref.date)
plot(p, main = 'Closing price')

p1 <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p1 <- p1 + geom_line()
p1 <- p1 + facet_wrap(~ticker, scales = 'free_y')

p1 <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close)) +
  + geom_line() + facet_wrap(~ticker, scales = 'free_y')

#Ferrari returns
rrace <- l.out$df.tickers$ret.closing.prices <-  xts(l.out$df.tickers$ret.closing.prices, order.by=l.out$df.tickers$ref.date)
rrace <- xts(l.out$df.tickers$ret.closing.prices,order.by=l.out$df.tickers$ref.date)

View(rrace)
plot(rrace)

#Nasdaq 100
tickers <- c('^NDX')

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,last.date = last.date,
                         cache.folder = file.path(tempdir(),'BGS_Cache') )

View(l.out$df.tickers$ret.closing.prices)

p2 <- xts(l.out$df.tickers$price.close, l.out$df.tickers$ref.date) #closing price

plot(p2, main = 'Closing price') #Plot Closing

p2 <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p2 <- p2 + geom_line()
p2 <- p2 + facet_wrap(~ticker, scales = 'free_y')
print(p2)

#Nasqad Index return
rndx <- l.out$df.tickers$ret.closing.prices <-xts(l.out$df.tickers$ret.closing.prices,  order.by=l.out$df.tickers$ref.date)

rndx <- xts(l.out$df.tickers$ret.closing.prices,  order.by=l.out$df.tickers$ref.date)

plot(rndx)

#Treasury Bill
tickers <- c('^IRX')

l.out <- BatchGetSymbols(tickers = tickers,
                           first.date = first.date,
                           last.date = last.date,
                           cache.folder = file.path(tempdir(), 'BGS_Cache') )

View(l.out$df.tickers$ret.closing.prices)

#closing prices
p3 <- xts(l.out$df.tickers$price.close, l.out$df.tickers$ref.date)
plot(p3, main = 'Closing price')

#tbill return
rtbill <- l.out$df.tickers$ret.closing.prices <- xts(l.out$df.tickers$ret.closing.prices,order.by=l.out$df.tickers$ref.date)
rtbill <- xts(l.out$df.tickers$ret.closing.prices,order.by=l.out$df.tickers$ref.date)
plot(rtbill)

#capm

fit <- lm(errace ~ erndx)
summary(fit)

plot(errace,type="l",col="green", ylab = "Excess Return",main = "Actual vs Fitted")
lines(fitted(fit),col="blue")

confint(fit)



