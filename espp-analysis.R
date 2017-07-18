require(quantmod)
require(PerformanceAnalytics)

ticker <- "EMC"
stockdata <- getSymbols(ticker, auto.assign = F)

# esppjan <- stockdata[.indexmon(stockdata)==0]
# esppjune <- stockdata[.indexmon(stockdata)==5]
# 
# espp <- rbind(esppjan, esppjune)
# 
# espp0 <- espp[.indexmday(espp)==0]
# espp1 <- espp[.indexmday(espp)==1]
# espp2 <- espp[.indexmday(espp)==2]
# espp3 <- espp[.indexmday(espp)==3]
# espp29 <- espp[.indexmday(espp)==29]
# espp30 <- espp[.indexmday(espp)==30]
# espp31 <- espp[.indexmday(espp)==31]
# 
# emcespp <- rbind(espp0, espp1, espp2, espp3, espp29, espp30, espp31)

mean <- mean(stockdata$EMC.Volume)
sd <- sd(stockdata$EMC.Volume)
highVolData <- NULL

for(i in 1:length(index(stockdata))) {
  if (stockdata[i,5] > (mean + (sd * 2))) {
    if (is.null(highVolData)) {
      highVolData <- stockdata[i]  
    } else {
      highVolData <- rbind(highVolData, stockdata[i], stockdata[i-1], stockdata[i+1])  
    }
    print(stockdata[i])   
    
  }
}

esppData <- rbind(stockdata["2010-12-31"], stockdata["2011-06-30"], stockdata["2011-12-30"],
                  stockdata["2012-06-29"], stockdata["2012-12-31"], stockdata["2013-06-28"],
                  stockdata["2014-01-31"], stockdata["2014-07-31"], stockdata["2015-01-30"],
                  stockdata["2015-07-31"])

esppDataP1 <- rbind(stockdata["2011-01-03"], stockdata["2011-07-01"], stockdata["2012-01-03"],
                  stockdata["2012-07-02"], stockdata["2013-01-02"], stockdata["2013-07-01"],
                  stockdata["2014-02-03"], stockdata["2014-08-01"], stockdata["2015-02-02"],
                  stockdata["2015-08-03"])


esppData$R <- NA
esppDataP1$R <- NA

#print(" *************************** ESPP DATA ************************")
for (i in 1:length(index(esppData))) {
  esppData$R[i] <- (esppData[i, 1] - esppData[i, 4]) / esppData[i, 4]
}

#print(" *************************** ESPP +1 Day DATA ************************")
for (i in 1:length(index(esppDataP1))) {
  esppDataP1$R[i] <- (esppDataP1[i, 1] - esppDataP1[i, 4]) / esppDataP1[i, 4]
}

esppDataP0P1 <- rbind(esppData, esppDataP1)
newXts <- NULL

for (i in seq(2, 20, 2)) {
  esppDataP0P1$R[i] <- (as.double(esppDataP0P1[i, 4]) - as.double(esppDataP0P1[i-1, 1])) / as.double(esppDataP0P1[i-1, 1])
  esppDataP0P1$R[i-1] <- NA
  
  if(is.null(newXts)) {
    newXts <- esppDataP0P1$R[i]
  } else {
    newXts <- rbind(newXts, esppDataP0P1$R[i])
  }
}

Return.cumulative(newXts$R)
Return.cumulative(esppDataP0P1$R)
