require(quantmod)
require(PerformanceAnalytics)

#Load Systematic investor toolbox for helpful functions
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

#Symbols<-c("XOM","MSFT","JNJ","GE","CVX","WFC","PG","JPM","VZ","PFE","T","IBM","MRK","BAC","DIS","ORCL","PM","INTC","SLB","MA","EMC")
Symbols<-c("EMC","MSFT","VMW", "GOOG", "AMZN")

#Set start date
start_date=as.Date("2011-01-01")

#Create New environment to contain stock price data
dataEnv<-new.env()

#download data          
getSymbols(Symbols, env=dataEnv, from=start_date)

#adjust all prices for div and splits
for(i in Symbols){
  print(i)
  dataEnv[[i]] <- adjustOHLC(dataEnv[[i]], use.Adjusted=T)
}

#helper function for extracting Closing price of getsymbols output and for date alignment 
bt.prep(dataEnv, align='remove.na')


#prices data
stock_prices <- dataEnv$prices
sma90mat <- "NA"
sma300mat <- "NA"
return_matrix <- stock_prices;

for(i in 1:length(stock_prices[1,])) {
  return_matrix[,i] <- NA
}

for(i in 1:length(stock_prices[1,])) {
  sma90 <- SMA(stock_prices[,i], n = 90)
  sma300 <- SMA(stock_prices[,i], n = 300)
  
  sma90mat <- cbind(sma90mat, sma90)
  sma300mat <- cbind(sma300mat, sma300)
  
  print(stock_prices[1,i])
  
  
  for(j in 300:length(sma90)) {
    if (sma90[j] > sma300[j]) {
      print(paste0("BUY ", " sma90: ", sma90[j], " sma300: ", sma300[j]))
      return_matrix[j,i] = stock_prices[j,i]
      
    } else {
      print(paste0("SELL ", " sma90: ", sma90[j], " sma300: ", sma300[j]))
      return_matrix[j,i] = NA
    }
  }
  
}

#calculate returns
stock_returns = Return.calculate(return_matrix, method = c("discrete"))

#Plot Performance for first three stocks
charts.PerformanceSummary(stock_returns,main='Stock Absolute Performance')


calcdatafor1stock <- function(ticker, PRINT){
  stock_data <- cbind(stock_prices[, ticker], sma90mat[, paste0(ticker, ".SMA.90")], sma300mat[, paste0(ticker, ".SMA.300")], return_matrix[, ticker])
  #changing column name
  names(stock_data)[names(stock_data) == paste0(ticker, ".1")] <- "strategy_return"
  if(PRINT == T) {
    chart.TimeSeries(stock_data, legend.loc = "bottomright")
  }
  
  cat("Stock Return: ", Return.cumulative(Return.calculate(stock_data[,ticker])), "\n")
  cat("Strategy Return: ", Return.cumulative(Return.calculate(stock_data[,"strategy_return"])), "\n")
  
  return(stock_data)
}

#print data for specific stock
#ticker <- "EMC"
#stock_data <- cbind(stock_prices[, ticker], sma90mat[, paste0(ticker, ".SMA.90")], sma300mat[, paste0(ticker, ".SMA.300")], return_matrix[, ticker])

#changing column name
#names(stock_data)[names(stock_data) == paste0(ticker, ".1")] <- "strategy_return"

#charts.PerformanceSummary(Return.calculate(stock_data), main='Stock Absolute Performance')
#chart.TimeSeries(stock_data, legend.loc = "bottomright")
