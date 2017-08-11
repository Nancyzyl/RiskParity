library(zoo)
library(xts)
library(TTR)

logreturn <- read.csv("log_local.csv")
rownames(logreturn) <- logreturn[, 1] 
catenum <- logreturn[, ncol(logreturn)]
logreturn <- logreturn[, -c(1, ncol(logreturn))]
# logreturn <- logreturn[-1, ] - logreturn[-(nrow(logreturn)), ]

riskbudget <- read.csv('riskbudget.csv')
riskbudget <- riskbudget[, 2]
costs <- read.csv('cost.csv')[, 2]
# riskbudget <- read.csv('risk2.csv')
# riskbudget <- drop(as.matrix(riskbudget))
parameter <- read.csv('ParaVol.csv')

module = 3
VolParity <- function(logdata, catenum, module, riskbudget, time.period, costs = rep(0, 38)){
  # attach(parameter)
  logdata <- logreturn
  logdata_na <- logdata
  logdata[is.na(logdata)] <- 0
  data <- exp(logdata) - 1
  kcate <- ncol(logdata)
  trade.date <- rownames(logdata)
  # time.name <- grep('^time.period', colnames(parameter), value = T)
  # time.period <- NULL
  # for(k in 1:length(time.name)){
  #   time.period <- c(time.period, parameter[time.name[k]])
  # }
  time.period <- c(60, 80, 100, 120, 140, 160, 180)
  time.control <- max(c(time.period), 60)
  signal <- weight.ts <- NULL
  if(module != 3){
    for(i in time.control:nrow(logdata)){
    signal <- rbind(signal, SignalSP(data = logdata_na,
                                     date.calculate = trade.date[i],
                                     time.period = time.period,
                                     time.volatility = rep(240, length(time.period)))
                    )
    }
    signal[is.na(signal)] <- 0
    signal.adjust <- SignalAdjust(signal = signal, module = module)
    } else {
      price <- apply(exp(logdata), 2, cumprod)
      price[is.na(logdata_na)] = NA
      for(i in time.control:nrow(logdata)){
        signal <- rbind(signal, SignalMA(data = price,
                                         date.calculate = trade.date[i],
                                         time.period = time.period)
                        )
      }
      signal[is.na(signal)] <- 0
      signal.adjust <- signal
    }
  # signal.adjust <- signal.adjust * kcate / catenum[time.control:nrow(logdata)]
  for(t in time.control:nrow(logdata)){
    optweight <- VolWeight(data = logdata, date.calculate = trade.date[t], days.vol = 60) 
    # optweight <- optweight * kcate / catenum[t] / 7
    weight.ts <- rbind(weight.ts, optweight  * riskbudget * signal.adjust[(t - time.control + 1), ])
    # weight.ts <- rbind(weight.ts, VolatilityMonitor(data = data,
    #                                                 date.calculate = trade.date[t],
    #                                                 weights = optweight * riskbudget * signal.adjust[(t - time.control + 1), ],
    #                                                 days.monitor = 60,
    #                                                 target.vol = 0.08)
    #                   )
  }
  rownames(signal.adjust) <- rownames(weight.ts) <- trade.date[time.control:nrow(logdata)]
  returns <- rowSums((as.matrix(data[((time.control + 1):nrow(data)), ]) * weight.ts[-nrow(weight.ts), ]) - TradeCost(weight.ts, catecosts = costs)[-nrow(weight.ts), ])
  returns <- returns * kcate / catenum[time.control:(nrow(logdata) - 1)] / 7
  performance <- PerformEva(returns)
  # detach(parameter)
  return(list(signal = signal, signal.adjust = signal.adjust, weight.ts = weight.ts, 
              returns = returns, performance = performance))
}

perform <- NULL
time.pe <- c(20,40,60,80,100,120,140,160,180,200,220,240)
for(k in 1:length(time.pe)){
  ans <- VolParity(logdata = logreturn, catenum = catenum, module = 3,
                   riskbudget = riskbudget, time.period = time.pe[k],
                   costs = costs)
  perform <- cbind(perform, ans$performance$sharperatio)
  # logsum <- cumsum(log(ans$returns + 1))
  # plot(logsum, type = 'l')
  # write.csv(ans$signal.adjust, paste('signal1_', k, '.csv'))
  # write.csv(ans$weight.ts, 'weight.ts1.csv')
  # write.csv(ans$finalweight,'finalweight1.csv')
  # write.csv(ans$returns,'returns1.csv')
  # plot(ans$performance$net.value, type = 'l')
  # perform <- rbind(perform, ans$performance[-(1:2)])
}

write.csv(perform, 'perform.csv')
