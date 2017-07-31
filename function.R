#####
Adjust <- function(x, t){
  if (abs(x) >= t){
    x <- t
  } else {
    x <- x
  }
  return(x)
}

SharpeRatio <- function(data, days.calculate, days.return, days.volatility){
  trade.date <- rownames(data)
  ind <- which(data == days.calculate)
  num.temp <- apply(data[(ind - days.return + 1):ind, ], 2, sum)
  den.temp <- apply(data[(ind - days.volatility + 1):ind, ], 2, sd) * sqrt(days.return)
  sharperatio <- num.temp / den.temp
  sharperatio <- apply(as.matrix(sharperatio), 1, Adjust, t = 2)
  return(sharperatio)
}

Signal <- function(data, days.calculate, time.period, time.volatility){
  ksiganl <- length(time.period)
  sharperatio <- NULL
  for(i in 1:ksignal){
    sharperatio <- rbind(sharperatio, SharpeRatio(data, days.calculate, 
                                                  days.return = time.period[i],
                                                  days.volatility = time.volatility[i])
                         )
    signal <- apply(sharperatio, 2, mean)
    signal <- apply(as.matrix(signal), 1, Adjust, t = 1)
    return(siganl)
  }
  
}

## n=1时还没改
RunSum <- function (x, n = 10) {
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
  is.na(result) <- c(1:(n - 1 + NAs))
  reclass(result, x)
}

Covariance <- function(data, days.calculate, days.covariance){
  trade.date <- rownames(data)
  ind <- which(data == days.calculate)
}
