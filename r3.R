require(quantmod)
require(PerformanceAnalytics)
require(corrplot)

#Load Systematic investor toolbox for helpful functions
setInternet2(TRUE)
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

getStocksPrices <- function(symbols, indices, start_date){
  data<-new.env()
  
  #download data 
  getSymbols(symbols, env=data, from=start_date)
  
  #adjust all prices for div and splits
  for(i in indices){
    print(i)
    data[[i]] <- adjustOHLC(data[[i]], use.Adjusted=T)
  }
  
  #helper function for extracting Closing price of getsymbols output and for date alignment 
  bt.prep(data, align='remove.na')
  
  #prices data
  price_matrix <- data$prices
  
  return(price_matrix)
}


# inputs - date, symbols
symbols <- c("^GSPC", "^HSI", "^GDAXI", "^FTSE", "^FCHI")
indices <- c("GSPC", "HSI", "GDAXI", "FTSE", "FCHI")
start_date=as.Date("2006-01-01")

#S&P500 (^GSPC)
#HS (^HIS)
#DAX (^GDAXI)
#FTSE (^FTSE)
#CAC (^FCHI)

prices <- getStocksPrices(symbols, indices, start_date)
returns <- CalculateReturns(prices)
corrmatrix <- cor(prices)

# plots
corrplot(corrmatrix, method = "number")
charts.PerformanceSummary(returns, main='Stock Absolute Performance')



