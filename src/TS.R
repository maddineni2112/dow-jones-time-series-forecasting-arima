if(!require('datasets')) {
  install.packages('datasets')
  library('datasets')
}

#remove.packages(c("forecast","rlang","cli","magrittr"))

#install.packages(c("rlang","magrittr","cli"), type="binary")
#install.packages("forecast", type="binary")
#install.packages("ggplot2", type ="binary")

if(!require('forecast')) {
  install.packages('forecast')
  library('forecast')
}
if(!require('stats')) {
  install.packages('stats')
  library('stats')
}
library(ggplot2)
AP <- ts(AirPassengers, start = c(1949,1), frequency = 12)

autoplot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

fit1<-arima(AP, order = c(1,0,0))
forecast<-predict(fit1, n.ahead=10)
summary(fit1)

c<-decompose(AP)
plot(c)

Acf(AP, lag.max = 120)          # specify lag parameter with value
Pacf(AP, lag.max = 120)		  # specify lag parameter with value


Airdiff1 = diff(AP, lag = 1, differences = 1)      # diff the time series in an attempt to make it stationary 
autoplot(Airdiff1) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 


Acf(Airdiff1, lag.max=120)
Pacf(Airdiff1, lag.max=120)



logAir=log2(AP)		   # if linear differencing does not work, try out a log transform on the raw data before applying differencing once again.
logAirdiff1 = diff(logAir, lag = 1, differences = 1)
autoplot(logAirdiff1) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

Acf(logAirdiff1, lag.max = 120)
Pacf(logAirdiff1, lag.max = 120)

logAirdiff2 = diff(logAir, lag = 1, differences = 2)
autoplot(logAirdiff2) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

Acf(logAirdiff2, lag.max = 120)
Pacf(logAirdiff2, lag.max = 120)
m1<-auto.arima(logAir)
summary(m1)
m2<-auto.arima(AP)
summary(m2)

