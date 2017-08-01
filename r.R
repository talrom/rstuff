require(quantmod)
require(PerformanceAnalytics)


ticker <- "EMC"
stockdata <- getSymbols(ticker, auto.assign = F)

names(stockdata)[1] <- "open"
names(stockdata)[2] <- "high"
names(stockdata)[3] <- "low"
names(stockdata)[4] <- "close"
names(stockdata)[5] <- "volume"
names(stockdata)[6] <- "adjusted"

sma90 <- SMA(stockdata$adjusted, n = 90)
sma300 <- SMA(stockdata$adjusted, n = 300)

stockdata$sma90 <- sma90
stockdata$sma300 <- sma300

stockdata$R <- "NA"


for(i in 300:length(sma90)) {
  if (sma90$SMA[i] > sma300$SMA[i]) {
    print(paste0("BUY ", index(stockdata[i]) ," " , ticker, ": ", stockdata$adjusted[i], " sma90: ", sma90[i], " sma300: ", sma300[i]))
    stockdata$R[i] <- stockdata$adjusted[i]
  } else {
    print(paste0("SELL ",index(stockdata[i]), " ", ticker, ": ", stockdata$adjusted[i], " sma90: ", sma90[i], " sma300: ", sma300[i]))
  }
}

ret <- Return.calculate(stockdata$R)

# chart options
#chartSeries(stockdata$adjusted)
#addSMA(90)
#addSMA(300, col = "blue")
#addMomentum(90)

# Add stock benchmark without strategy
ret$benchmark.R <- Return.calculate(stockdata$adjusted)

# Add market benchmark (SPY)
getSymbols("SPY");
ret$spyR <- Return.calculate(SPY$SPY.Adjusted)

# Changing ret col names
names(ret) <- c("strat.R", "benchmark.R", "MRKT.R");

print("************************")
print(Return.cumulative(ret))
print(SharpeRatio.annualized(ret))
print(SharpeRatio(ret))

charts.PerformanceSummary(ret, main='Stock Absolute Performance',legend.loc="bottomright")

