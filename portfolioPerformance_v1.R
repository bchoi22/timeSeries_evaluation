
rm(list = ls())
install.packages("Quandl")
Quandl.api_key('pyja4MxF_1bEXcZkUSBY')

library(Quandl)
library(ggplot2)
require(forecast)
library(KFAS)
library(TSPred)
library(tibble)

start <- "2015-01-02" # starting date
end <- "2016-12-31"   # ending date
freq <- "daily"
#ticker <- c("GOOG", "AMZN", "MSFT", "AAPL", "FB", "BABA")

#View(Quandl('BATS/EDGA_BABA'))
goog <- Quandl('WIKI/GOOG', collapse=freq, start_date=start, end_date=end)#[,c("Date", "Close")]
amzn <- Quandl('WIKI/AMZN', collapse=freq, start_date=start, end_date=end)#[,c("Date", "Close")]
msft <- Quandl('WIKI/MSFT', collapse=freq, start_date=start, end_date=end)#[,c("Date", "Close")]
aapl <- Quandl('WIKI/AAPL', collapse=freq, start_date=start, end_date=end)#[,c("Date", "Close")]
fb <- Quandl('WIKI/FB', collapse=freq, start_date=start, end_date=end)#[,c("Date", "Close")]
#baba <- Quandl('BATS/BATS_BABA', collapse="monthly", start_date=start, end_date=end)[,c("Date", "Close")]

gmean <- mean(goog$Close)
gClose <- goog$Close - gmean

amean <- mean(amzn$Close)
amClose <- amzn$Close - amean

mmean <- mean(msft$Close)
mClose <- msft$Close - mmean

apmean <- mean(aapl$Close)
apClose <- aapl$Close - apmean

fbmean <- mean(fb$Close)
fbClose <- fb$Close - fbmean


techPortfolio = data.frame(gClose, amClose, mClose, apClose, fbClose)
View(techPortfolio)

df <- data.frame(matrix(ncol = 2, nrow = 0))
i=0
j=1
while (i < nrow(techPortfolio)){
  subPortfolio = techPortfolio[i:(i+7),]
  C = cov(subPortfolio)
  print(C)
  weight = c(rep(1,nrow(C)))
  variance = weight%*%C%*%weight
  
  df[nrow(df) + 1,1] = j
  df[nrow(df),2] = c(log(sqrt(variance)))
  i = i+8
  j=j+1
  if (i > nrow(techPortfolio)){
    break;
  }
}
names(df)[2] <- "volatility"

portfolio_ts = ts(df, start=1, end = nrow(df)-8, frequency = 1)
portfolio_ts.test = ts(df, start=nrow(df)-7, end = nrow(df)-3, frequency = 1)
View(portfolio_ts)
View(portfolio_ts.test)

acf(portfolio_ts[,2])
ndiffs(portfolio_ts[,2])

plot(portfolio_ts, ylab="standard deviation", xlab = "Week")
title("Portfolio Standard Deviation from 2015-16")

#best_port = auto.arima(portfolio_ts)
best_port = arima(portfolio_ts[,2], order = c(1,1,0))
best_port
summary(best_port)
coefic = coef(best_port)

#myArimaModel = SSMarima(ar = coefic, ma = NULL, d = 0, stationary = TRUE, n = nrow(portfolio_ts))
#myModel = SSModel(matrix(portfolio_ts) ~ matrix(myArimaModel))
#class(myArimaModel)
#class(df)
#out <- KFS(myArimaModel)

predict(portfolio_ts[,2], n.ahead = 5)
portPredict = forecast(object = best_port, h=10)
plot(portPredict, ylab="standard deviation", xlab = "Week")
df.home = model.matrix(~portfolio_ts)
df.cont = model.matrix(~portfolio_ts.test)
#fArimaKF <- fittestArimaKF(df[1:50,2], df[51:55,2])
#fArimaKF <- fittestArimaKF(portfolio_ts[,2], portfolio_ts.test[,2])



