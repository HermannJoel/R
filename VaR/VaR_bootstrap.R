library(mosaic)
library(fImport)
library(dplyr)
library(readr)
library(reshape2)
library(ggplot2)
library(quantmod)
library(Quandl)
Quandl.api_key("HeSenjS7xyCu9Kw3-sBs")
setwd("C:/Users/nherm/Downloads/VaR/VaR")

mystocks =c("SPY", "TLT", "LQD", "DBC", "VNQ")
myprices = yahooSeries(mystocks, from="2010-12-31", to="2020-12-31")
getwd()
myreturns <- read.csv("log_returns.csv", header=TRUE, row.names="Date")
head(myreturns)
#To view the returns as a joint distribution
pairs(myreturns)
plot(myreturns[,3], type = "l")

#Simulate one day change in our port
#we will allocate 20% to each asset.
totalwealth = 200000
weights = c(0.090, 0.080, 0.083, 0.083, 0.083, 0.083, 
            0.083, 0.083, 0.083, 0.083, 0.083, 0.083)#percentage of the wealth we put in each stock
sum(weights)

#how much money do we have in each stock
holdings= weights * totalwealth
#sample a random return from the empirical joint dist
#this simulate a random day
return.today = resample(myreturns, 1, orig.ids=FALSE)
return.today
#update the value of holding
holdings = holdings + holdings*return.today
holdings

#We loop over 20 trading day(4 weeks)
totalwealth = 200000
horizon = 20
weights = c(0.090, 0.080, 0.083, 0.083, 0.083, 0.083, 
            0.083, 0.083, 0.083, 0.083, 0.083, 0.083)
holdings = weights * totalwealth
wealthtracker = rep(0, horizon)

for (today in 1:horizon) {
  return.today = resample(myreturns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  totalwealth = sum(holdings)
  wealthtracker[today] = totalwealth
}
totalwealth
plot(wealthtracker)
title(main = "wealth tracker",col = "red", font = 1)
abline(h=240000, col="green")

#Now simulate many different possible 4 weeks trading period(20 days)
#we out pout the total wealth after 4 weeks
sim = do(2000)*{
  totalwealth = 200000
  weights = c(0.090, 0.080, 0.083, 0.083, 0.083, 0.083, 
              0.083, 0.083, 0.083, 0.083, 0.083, 0.083)
  holdings = weights * totalwealth
  for (today in 1:horizon) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
  }
 totalwealth 
}
#visualize and summarize the results
hist(sim$result, 50)
mean(sim$result)
sd(sim$result)
#We can calculate an expected utility
#if u(w) = log(w), then:
mu_utility = mean(log(sim$result))
mu_utility

#VaR
profit = sim$result - 200000
hist(profit, 50)
#We calculate the 5% VaR
#It represent the 5% quantile of the p/l distribution
qdata(profit, 0.05)

VaR05 = qdata(profit, 0.05)[1]
abline(v=VaR05, col="red", lwd=2)
abline(v=mean(profit), col="green", lwd=2)#mean Expected return of the port

?getSymbols
mydata = Quandl("FRED/GDP.1", start_date="2015-01-01", 
                end_date="2021-12-31")
tail(mydata)
mydata = Quandl("FRED/GDP", start_date="2015-01-01", 
                end_date="2021-12-31")

quandl_api_key("HeSenjS7xyCu9Kw3-sBs")
myStocks <- c("WIKI/AAPL", "WIKI/CRM", "WIKI/TSLA")
myData = Quandl(myStocks, start_date="2018-03-01", 
                end_date="2021-03-01")
head(myData)
tail(myData)
?grep
#----------------Portfolio resampling
computereturns = function(series) {
  
  mycols = grep('Adj. Close', colnames(series))
  myorder = order(rownames(series))
  series = series[myorder,]
  
  closingprice = series[,mycols]
  
  N = nrow(closingprice)
  myrownames = rownames(series)[1:(N-1)]
  
  percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
  
  mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
  
  mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
  
  colnames(percentreturn) = mynames
  rownames(percentreturn) = myrownames
  
  na.omit(percentreturn)
  
}
options(scipen = 999)
my_returns <- computereturns(myData)
my_returns

#stock split list and date
sp_list <- 
#spx500 list
library(BatchGetSymbols)
sp500 <- GetSP500Stocks()
sp500$Tickers
spx <- sp500$Tickers
spx
str(spx)
#Random sampling
rs <- sample(spx, 1, replace=T)

Return.cumulative(managers[,1:8],geometric=FALSE)

df_wide <- pivot_wider(stocks, names_from = symbol, values_from = adjusted)


myStocks <-lapply(c("AAPL", "CRM", "TSLA"), function(x) {getSymbols(x, 
                                                             from = "2019/01/01", 
                                                             to = "2020/12/31",
                                                             auto.assign=TRUE)})
names(myStocks) <- c("AAPL", "CRM", "TSLA")
head(myStocks$AAPL)

adjustedPrices <- lapply(myStocks, Ad)
adjustedPrices <- do.call(merge, adjustedPrices)
head(adjustedPrices)
#Event study
data(daEsa)
head(daEsa)
?evReturn
# event analysis for one firm and one event window
hh <- evReturn(y = daEsa, firm = "wpp", 
               y.date = "date", index = "sp500", est.win = 250, digits = 3,
               event.date = 19990505, event.win = 5)
hh; plot(hh)

# event analysis for many firms and one event window
hh2 <- update(hh, firm = c("tin", "wy", "pcl", "pch")); hh2


