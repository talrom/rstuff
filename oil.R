require(quantmod)
require(PerformanceAnalytics)
require(Quandl)

ticker <- "SPY"
stockdata <- getSymbols(ticker, auto.assign = F, from="2007-1-1")

firstSPY <- do.call(rbind, lapply(split(stockdata, "months"), first))

mydata = Quandl("OFDP/FUTURE_CL1", type="xts", start_date="2007-1-1")

# get the first business day of each month
first <- do.call(rbind, lapply(split(mydata, "months"), first))

ret <- first$Settle
names(ret)[1] <- "MRKT"
ret$MRKT.R <- Return.calculate(ret$MRKT)
ret$strat <- NA

for(i in 1:length(ret$MRKT)) {
  if ((.indexmon(ret[i]) >= 3 && .indexmon(ret[i]) <= 8)) {
    print(paste0("BUY at", index(ret[i])))
    ret$strat[i] = ret$MRKT[i]
  } else {
    
  }
}

ret$strat.R <- Return.calculate(ret$strat)
ret$SPY.R <- Return.calculate(firstSPY$SPY.Adjusted)

result <- cbind(ret$MRKT.R, ret$strat.R, ret$SPY.R)
charts.PerformanceSummary(result)
