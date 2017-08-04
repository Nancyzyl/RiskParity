#####
## 将值调整在[-t,t]之间 ##
Adjust <- function(x, t){
  if (abs(x) >= t){
    x <- sign(x) * t
  } else {
    x <- x
  }
  return(x)
}

## 计算夏普比 ##
SharpeRatio <- function(data, date.calculate, days.return, days.volatility){
  # data:计算数据
  # data.calculate:计算日期（计算哪天的夏普比）
  # days.return:分子时长
  # days.volatility:分母时长
  trade.date <- rownames(data)
  # 定位所计算日期在data中的位置
  ind <- which(trade.date == date.calculate)
  # 分子
  num.temp <- apply(data[(ind - days.return + 1):ind, ], 2, sum)
  # 分母
  den.temp <- apply(data[(ind - days.volatility + 1):ind, ], 2, sd) * sqrt(days.return)
  sharperatio <- num.temp / den.temp
  # 将夏普比的取值调整在[-2, 2]之间
  sharperatio <- apply(as.matrix(sharperatio), 1, Adjust, t = 2)
  return(sharperatio)
}

## 计算信号 ##
Signal <- function(data, date.calculate, time.period, time.volatility){
  # time.period: days.return的集合
  # time.volatility:days.volatility的集合
  ksignal <- length(time.period)
  sharperatio <- NULL
  for(i in 1:ksignal){
    sharperatio <- rbind(sharperatio, SharpeRatio(data, date.calculate, 
                                                  days.return = time.period[i],
                                                  days.volatility = time.volatility[i])
                         )
  }
  signal <- apply(sharperatio, 2, mean)
  signal <- apply(as.matrix(signal), 1, Adjust, t = 1)
  return(signal)
}

## 调整信号 ##
# module = 1 则为指数平滑
# module !=1 则为{-1, 0, 1}三个信号
SignalAdjust <- function(signal, module = 1){
  signal.adjust <- matrix(NA, ncol = ncol(signal), nrow = nrow(signal))
  if(module == 1){
    signal.adjust[1, ] <- signal[1, ]
    signal.adjust[-1, ] <- signal[-1, ] * 0.95 + signal[-nrow(signal), ] * 0.05
  } else {
    signal.adjust <- sign(signal)
    signal.adjust[-1, ][sign(signal)[-1, ] * sign(signal)[-nrow(signal), ] == -1] <- 0
  }
  return(signal.adjust)
}

## 移动求和（除以n则为移动平均） ##
RunSum <- function (x, n = 10, cumulative = FALSE) {
  # n:移动步长
  x <- try.xts(x, error = as.matrix)
  if (n < 1 || n > NROW(x)) 
    stop("Invalid 'n'")
  if (NCOL(x) > 1) {
    stop("ncol(x) > 1. runSum only supports univariate 'x'")
  }
  NAs <- sum(is.na(x))
  if (NAs > 0) {
    if (any(is.na(x[-(1:NAs)]))) 
      stop("Series contains non-leading NAs")
    if (NAs + n > NROW(x)) 
      stop("not enough non-NA values")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs
  result <- double(NROW(x))
  if (cumulative) {
    result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])
  }
  else {
    result[(n + beg - 1)] <- sum(x[beg:(n + beg - 1)])
    result <- .Fortran("runsum", ia = as.double(x[beg:NROW(x)]), 
                       lia = as.integer(len), n = as.integer(n), oa = as.double(result[beg:NROW(x)]), 
                       loa = as.integer(len), PACKAGE = "TTR", DUP = TRUE)$oa
    result <- c(rep(NA, NAs), result)
  }
  is.na(result) <- c(min(1, (n - 1 + NAs)):(n - 1 + NAs))
  reclass(result, x)
}

## 计算协方差矩阵 ##
Covariance <- function(data, date.calculate, days.covariance, kcovariance = 1){
  # days.covariance:计算过去多少天收益的协方差
  # kcovariance:几天数据rolling
  trade.date <- rownames(data)
  ind <- which(trade.date == date.calculate)
  data.cal <- data[(ind - days.covariance - kcovariance + 2):ind, ]
  data.cal <- na.omit(apply(data.cal, 2, RunSum, n = kcovariance))
  covariance <- cov(data.cal)
  return(covariance)
}

## VolParity求权重 ##
VolWeight <- function(data, date.calculate, days.vol){
  # days.vol:计算过去多少个交易日的波动率
  trade.date <- rownames(data)
  ind <- which(trade.date == date.calculate)
  weight <- 0.02 / apply(data[(ind - days.vol + 1):ind, ], 2, sd)
  return(weight)
}

## RiskParity求权重 ##
# Tips:需在python中安装numpy、pandas、cvxopt模块
OptWeightPy <- function(covariance){
  write.csv(covariance, 'cov.csv', row.names = F)
  ###调用PYTHON
  ans <- py_run_file('1.py')
  weights <- as.vector(ans$a)
  return(weights)
}

## 波动率监控 ##
VolatilityMonitor <- function(data, date.calculate, weights, days.monitor, target.vol){
  # days.monitor:监控时计算过去多长时间的波动率
  trade.date <- rownames(data)
  ind <- which(trade.date == date.calculate)
  # 以当天计算得到的权重，计算过去N天的波动率
  real.vol <- sd(as.matrix(data[(ind - days.monitor + 1):ind, ]) %*% weights)
  if(real.vol > target.vol){
    weights <- weights * target.vol / real.vol
  }
  return(weights)
}

## 交易成本 ##
TradeCost <- function(weights, catecosts){
  # catecosts:每品种大约的交易成本比例，包括佣金和滑点
  # 计算头寸的改变
  netposition <- abs(weights - rbind(rep(0, ncol(weights)), weights[-nrow(weights), ]))
  costs <- netposition %*% catecosts
  return(costs)
}

## 绩效评估 ##
PerformEva <- function(return){
  # return: 每日收益率序列
  max.value <- NULL
  net.value <- cumprod(1 + return)
  for(i in 1:(length(return) - 1)) {
    max.value[i] <- net.value[i + 1] / max(net.value[1:(i + 1)]) - 1
  }
  max.drawback <- -min(max.value)
  # 算法1：每日收益率取平均
  # return.annually <- mean(return) * 240
  # 算法2：最后一个交易日的净值求年化
  return.annually <- (net.value[length(net.value)] - net.value[1]) / length(return) * 240
  vol.annually <- sd(return) * sqrt(240)
  sharperatio <- return.annually / vol.annually
  calmarratio <- return.annually / max.drawback
  return(list(net.value = net.value, max.value = max.value,
              return.annually = return.annually, max.drawback = max.drawback, 
              vol.annually = vol.annually, sharperatio = sharperatio, calmarratio = calmarratio))
}
