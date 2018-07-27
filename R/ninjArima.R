#' @title Returns the optimal (p,k,q) values in ARIMA Model.
#' @description This package is used to get the optimal autoregression, partial autoregression and difference required for the ARIMA model without going through the difficulties of understanding the jargons and how to get the values. Simply give the Time Series as input and get the values of p,k,q for the best ARIMA Model.

#' @param ts
#' @return pkq
#' @export ninjArima
ninjArima <- function(ts){
  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  # Identifying Difference
  k = 0
  tser = ts
  repeat{
    test <- adf.test(tser)
    if(round(test$p.value,2) < 0.05){
      break()
    } else {
      k = k + 1
      tser <- diff(tser, differences = 1)
    }
  }


  # Identifying Autocaorrelation lags or MA
  repeat{
    acf_df <- acf_fun(ts,k)
    if((acf_df$Val[2] < -0.5) & (k != 0)){
      k = k - 1
    } else {
      break
    }
  }

  thresh = c(1,-1)*1.96/sqrt(length(ts))

  lagacf <- lagfun(acf_df,thresh)
  lagacf = lagacf - 1

  # Identifying Partial Autocorrelation lags or AR
  pacf_df <- pacf_fun(ts,k)

  lagpacf <- lagfun(pacf_df, thresh)

  # pkq <- c(lagpacf,k,lagacf)
  # return(pkq)

  start = attributes(ts)$tsp[1]
  end = attributes(ts)$tsp[2]
  freq = attributes(ts)$tsp[3]
  inTrain <- trunc(75/100*(length(ts)/freq))
  train <- window(ts, start = start, end = (start + inTrain - 1), frequency = freq)
  test <- window(ts, start = (start + inTrain), end = end, frequency = freq)

  modelAR <- Arima(ts, order = c(lagpacf,k,0))
  modelMA <- Arima(ts, order = c(0,k,lagacf))
  modelARMA <- Arima(ts, order = c(lagpacf,k,lagacf))

  fut <- length(ts) - inTrain
  predAR <- forecast(modelAR, h = fut)
  predMA <- forecast(modelMA, h = fut)
  predARMA <- forecast(modelARMA, h = fut)

  rmseAR <- rmse(c(test), predAR$mean)
  rmseMA <- rmse(c(test), predMA$mean)
  rmseARMA <- rmse(c(test), predARMA$mean)

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


lagfun <- function(df,thresh){

  #To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  flag = 0
  final = 0
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
  }
  if(final == 0){
    return(final)
  } else {
    return(final)
  }
}
