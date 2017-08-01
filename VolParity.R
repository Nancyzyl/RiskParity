library(zoo)
library(xts)
library(TTR)

logreturn <- read.csv("logreturn.csv")
rownames(logreturn) <- logreturn[, 1]
logreturn <- logreturn[, -1]
logreturn <- logreturn[-1, ] - logreturn[-(nrow(logreturn)), ]
return <- exp(logreturn) - 1

riskbudget <- read.csv('riskbudget.csv')
riskbudget <- riskbudget[, 2]
parameter <- read.csv('ParaRisk.csv')

VolParity <- function(data, riskbudget, parameter){
  attach(parameter)
  trade.date <- rownames(data)
  time.period <- c(time.period1, time.period2, time.period3)
  signal <- weight.ts <- NULL
  for(i in time.period3:nrow(data)){
    signal <- rbind(signal, Signal(data,
                                   date.calculate = trade.date[i],
                                   time.period = time.period,
                                   time.volatility = rep(time.period3, 3))
                    )
    optweight <- VolWeight(data, date.calculate = trade.date[i], days.vol)
    weight.ts <- rbind(weight.ts, VolatilityMonitor(data, 
                                                    date.calculate = trade.date[i],
                                                    weights = optweight, 
                                                    days.monitor, 
                                                    target.vol)
                       )
  }
  rownames(signal) <- rownames(weight.ts) <- trade.date[time.period3:nrow(data)]
  finalweight <- signal * weight.ts
  finalweight <- t(apply(finalweight, 1, function(x) { x * riskbudget}))
  returns <- rowSums(finalweight[-nrow(finalweight), ] * as.matrix(data[((time.period3 + 1):nrow(data)), ]))
  performance <- PerformEva(returns)
  detach(parameter)
  return(list(signal = signal, weight.ts = weight.ts, finalweight = finalweight,
              returns = returns, performance = performance))
}

ans <- VolParity(return, riskbudget, parameter)
logsum <- cumsum(log(ans$returns + 1))
plot(logsum, type = 'l')
write.csv(ans$signal, 'signal.csv')
write.csv(ans$weight.ts, 'weight.ts.csv')
write.csv(ans$finalweight,'finalweight.csv')
plot(ans$performance$net.value)
