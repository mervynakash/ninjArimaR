#' @title Returns the optimal (p,k,q) values in ARIMA Model.
#' @description This package is used to get the optimal autoregression, partial autoregression and difference required for the ARIMA model without going through the difficulties of understanding the jargons and how to get the values. Simply give the Time Series as input and get the values of p,k,q for the best ARIMA Model.

#' @param ts
#' @return pkq
#' @export ninjArima
ninjArima <- function(ts){
  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  # Loading Libraries


  # Identifying Difference
  k = 0
  tser = ts
  repeat{
    test <- tseries::adf.test(tser)
    if(round(test$p.value,2) < 0.05){
      break()
    } else {
      k = k + 1
      tser <- diff(tser, differences = 1)
    }
  }

  thresh = c(1,-1)*1.96/sqrt(length(ts))

  # Identifying Autocaorrelation lags or MA
  repeat{
    acf_df <- acf_fun(ts,k)
    if((acf_df$Val[2] < -0.5) & (k != 0)){
      k = k - 1
    } else {
      break
    }
  }

  repeat{
    acf_df <- acf_fun(ts,k)
    if(nrow(acf_df[which(acf_df$Val > thresh),]) > 7){
      k = k + 1
    } else {
      break
    }
  }


  lagacf <- lagfun(acf_df,thresh,1)
  if(lagacf == 0){
    lagacf = 0
  } else {
    lagacf = lagacf - 1
  }


  # Identifying Partial Autocorrelation lags or AR
  pacf_df <- pacf_fun(ts,k)

  lagpacf <- lagfun(pacf_df, thresh,2)

  # pkq <- c(lagpacf,k,lagacf)
  # return(pkq)

  start = attributes(ts)$tsp[1]
  end = attributes(ts)$tsp[2]
  freq = attributes(ts)$tsp[3]
  inTrain <- trunc(75/100*(length(ts)/freq))
  train <- window(ts, start = start, end = (start + inTrain), frequency = freq)
  test <- window(ts, start = (start + inTrain), end = end, frequency = freq)

  modelAR <- forecast::Arima(ts, order = c(lagpacf,k,0))
  modelMA <- forecast::Arima(ts, order = c(0,k,lagacf))
  modelARMA <- forecast::Arima(ts, order = c(lagpacf,k,lagacf))

  fut <- length(ts) - inTrain
  predAR <- forecast::forecast(modelAR, h = fut)
  predMA <- forecast::forecast(modelMA, h = fut)
  predARMA <- forecast::forecast(modelARMA, h = fut)

  rmseAR <- Metrics::rmse(c(test), predAR$mean)
  rmseMA <- Metrics::rmse(c(test), predMA$mean)
  rmseARMA <- Metrics::rmse(c(test), predARMA$mean)

  models <- c("AR" = rmseAR, "MA" = rmseMA,"ARMA" = rmseARMA)
  best <- names(models[which.min(models)])

  if(best == "AR"){
    pkq <- c(lagpacf,k,0)
    return(pkq)
  } else if (best == "MA"){
    pkq <- c(0,k,lagacf)
    return(pkq)
  } else {
    pkq <- c(lagpacf,k,lagacf)
    return(pkq)
  }

}


pacf_fun <- function(tser,k){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  if(k != 0){
    tsnew <- diff(tser, differences = k)
  } else {
    tsnew <- tser
  }
  q <- pacf(tsnew, plot = F, lag.max = 10)
  pacf_df <- data.frame("Lag" = q$lag, "Val" = q$acf)
  return(pacf_df)
}


acf_fun <- function(tser,k){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  if(k != 0){
    tsnew <- diff(tser, differences = k)
  } else {
    tsnew <- tser
  }
  p <- acf(tsnew, plot = F, lag.max = 10)
  acf_df <- data.frame("Lag" = p$lag, "Val" = p$acf)
  return(acf_df)
}


lagfun <- function(df,thresh,k){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  flag = 0
  final = 0
  mean = 0
  pos = 0
  for(i in seq(1,nrow(df))){
    if(flag == 1){
      if(lagnum > 0){
        if(df$Val[i] < 0){
          final = i
          break
        }
      } else {
        if(df$Val[i] > 0){
          final = i
          break
        }
      }
    }
    flag = 0
    if(df$Val[i] > 0){
      if(df$Val[i] >= thresh[1]){
        lagnum = df$Val[i]
        flag = 1
      }
    } else {
      if(df$Val[i] <= thresh[2]){
        lagnum = df$Val[i]
        flag = 1
      }
    }
    if(i > 1){
      if(mean < abs(df$Val[i] - df$Val[i-1])){
        mean = abs(df$Val[i] - df$Val[i-1])
        pos = i
        flag = 1
      }
    }
  }

  if(final == 0){
    return(0)
  } else {
    if(pos < final){
      if(k == 1)
        return(pos)
      else
        return(pos - 1)
    } else {
      if(k == 1)
        return(final)
      else
        return(final - 1)
    }
  }
}
