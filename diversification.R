# This contains some code used to calculate correlations between certain assets, for diversification purposes

library(quantmod)

# pick this date because thats when VTIP data is available
ltpz = getSymbols("LTPZ", "VTIP", from="2012-10-16")
vtip = getSymbols("VTIP", from="2012-10-16")
tip = getSymbols("TIP", from="2012-10-16")
spy = getSymbols("SPY", from="2012-10-16")

# TIPS
cor(dailyReturn(SPY), dailyReturn(LTPZ))
cor(dailyReturn(SPY), dailyReturn(TIP))
cor(dailyReturn(SPY), dailyReturn(VTIP))

# REITs
getSymbols("VNQ", from="2012-10-16")
cor(dailyReturn(SPY), dailyReturn(VNQ))

# Bonds
getSymbols("VCIT", from="2012-10-16")
getSymbols("BND", from="2012-10-16")
cor(dailyReturn(VCIT), dailyReturn(SPY))
cor(dailyReturn(BND), dailyReturn(SPY))

# Equity
getSymbols("VWO", from="2012-10-16")
getSymbols("IEFA", from="2012-10-16")
cor(dailyReturn(SPY), dailyReturn(VWO))
cor(dailyReturn(SPY, subset="2012-10-24/"), dailyReturn(IEFA))
cor(dailyReturn(LTPZ), dailyReturn(VWO))

quarterlyCorrelation = function(x, y){
  # x and y are xts objections from getSymbols
  xy = merge(x, y)
  xy$returnx = dailyReturn(x)
  xy$returny = dailyReturn(y)
  cor2 = function(z) {
    cor(z$returnx, z$returny)
  }
  apply.quarterly(xy, cor2)
}

# To help pick between REITs
plot(quarterlyCorrelation(SPY, LTPZ))
lines(quarterlyCorrelation(SPY, TIP), col="red")
lines(quarterlyCorrelation(SPY, VTIP), col="blue")
