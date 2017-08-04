library(zoo)
library(xts)
library(TTR)
setwd('/Users/bitwjg/Desktop')
logreturn <- read.csv("logreturn.csv")
rownames(logreturn) <- logreturn[, 1]
logreturn <- logreturn[, -1]
logreturn <- logreturn[-1, ] - logreturn[-(nrow(logreturn)), ]
#return <- exp(logreturn) - 1
# write.csv(return,'returncate.csv')
# logreturn <- read.csv('logreturntest.csv')
# rownames(logreturn) <- logreturn[, 1]
# logreturn <- logreturn[, -1]

riskbudget <- read.csv('riskbudget.csv')
riskbudget <- riskbudget[, 2]
# riskbudget <- read.csv('risk2.csv')
# riskbudget <- drop(as.matrix(riskbudget))
parameter <- read.csv('ParaVol.csv')

perform <- NULL
VolParity <- function(logdata, riskbudget, parameter){
  attach(parameter)
  data <- exp(logdata) - 1
  trade.date <- rownames(data)
  time.period <- c(time.period1, time.period2, time.period3)
  signal <- weight.ts <- NULL
  for(i in time.period3:nrow(logdata)){
    print(i)
    signal <- rbind(signal, Signal(data = logdata,
                                   date.calculate = trade.date[i],
                                   time.period = time.period,
                                   time.volatility = rep(time.period3, 3))
                    )
  }
  signal.adjust <- SignalAdjust(signal = signal)
  for(i in time.period3:nrow(logdata)){
    optweight <- VolWeight(data = logdata, date.calculate = trade.date[i], days.vol)
   # weight.ts <- rbind(weight.ts, optweight)
    weight.ts <- rbind(weight.ts, VolatilityMonitor(data = data,
                                                   date.calculate = trade.date[i],
                                                   weights = optweight * riskbudget * signal.adjust[(i - time.period3 + 1), ],
                                                   days.monitor = days.monitor,
                                                   target.vol = target.vol)
    )
  }
  rownames(signal.adjust) <- rownames(weight.ts) <- trade.date[time.period3:nrow(logdata)]
  #weight.temp <- signal.adjust * weight.ts
  #finalweight <- t(apply(weight.temp, 1, function(x) { x * riskbudget}))
  finalweight <- weight.ts
  returns <- rowSums((as.matrix(data[((time.period3 + 1):nrow(data)), ]) * finalweight[-nrow(finalweight), ]))
  performance <- PerformEva(returns)
  detach(parameter)
  return(list(signal = signal, signal.adjust = signal.adjust, weight.ts = weight.ts, finalweight = finalweight,
              returns = returns, performance = performance))
}
for(k in 1:nrow(parameter)){
  ans <- VolParity(logreturn, riskbudget, parameter[k, ])
  # logsum <- cumsum(log(ans$returns + 1))
  # plot(logsum, type = 'l')
  # write.csv(ans$signal.adjust, 'signal1.csv')
  # write.csv(ans$weight.ts, 'weight.ts1.csv')
  # write.csv(ans$finalweight,'finalweight1.csv')
  # write.csv(ans$returns,'returns1.csv')
  # plot(ans$performance$net.value, type = 'l')
  perform <- rbind(perform, ans$performance[-(1:2)])
}

write.csv(perform, 'perform.csv')
