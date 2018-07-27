# Why use ninjArimaR?
The idea of this package is pretty simple. Input the time series variable in the ```ninjArima()``` method and in return this package will help you get the optimal AR order, degree of differencing and MA order. Now just use this returned value in an ```arima(ts, order = <return>)``` model and predict your time series.  

This package is exculsively for R Programmers who have  trouble understanding the ARIMA model and if the time series data set is quite huge then the ```auto.arima()``` method of the ```library(forecast)``` will take a huge amount of time to execute. 

# Installation
To get the current development version from github:
```R
install.packages("devtools")
devtools::install_github("mervynakash/ninjArimaR")
```
# Functions:
```ninjArima(<ts>)``` : The method takes a time series data as an input and returns you the optimal order to put in ARIMA model. Let's take a look in an example.

```R
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingsSeries <- ts(kings)
order <- ninjArima(kingsSeries)

model <- arima(kingsSeries, order = order)
pred_arima <- predict(model, n.ahead = 10)
print(pred_arima)
```

# Conclusion
There will be a bugs and errors and I would be highly honored if you developers would mail me at ```mervyn.akash10@gmail.com```. Any type of feedback and criticism is appreciated and hope to improve my algorithm for the benefits of R community. 

Thank You.
Happy Coding!!!!
