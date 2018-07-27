# Why use ninjArimaR?
The idea of this package is pretty simple. Input the time series variable in the ```ninjArima()``` method and in return this package will help you get the optimal AR order, degree of differencing and MA order. Now just use this returned value in an ```arima(ts, order = <return>)``` model and predict your time series.  

This package is exculsively for R Programmers who have  trouble understanding the ARIMA model and if the time series data set is quite huge then the ```auto.arima()``` method of the ```library(forecast)``` will take a huge amount of time to execute. 

# Installation
To get the current development version from github:
```R
install.packages("devtools")
devtools::install_github("mervynakash/ninjArimaR")
```
