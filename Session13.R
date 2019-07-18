acfVal<-ARMAacf((ar=c(0.8)), lag.max = 10)
plot(acfVal,type = "h", xlab = "Lag")

acfVal<-ARMAacf((ar=c(0.6,0.3)), lag.max = 10)
plot(acfVal,type = "h", xlab = "Lag")

acfVal<-ARMAacf((ma=c(0.6,0.3)), lag.max = 10)
plot(acfVal,type = "h", xlab = "Lag")

acfVal<-ARMAacf((ma=c(0.6,0.8,0.4,0.8)), lag.max = 10)
plot(acfVal,type = "h", xlab = "Lag")

xt=arima.sim(n=10000, list(ar=c(0.8897,-0.4858), ma=c(-0.2279,0.2488), sd=sqrt(0.1796)))
arima(xt, order = c(2,0,2))

# An ARIMA simulation
ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200)
ts.plot(ts.sim)


# Random Walk
ts.sim <- arima.sim(list(order = c(0,1,0)), n = 200)
ts.plot(ts.sim)

##############################


library(tseries)
library(forecast)

data(tcm)

?tcm

#Plot the data corresponding to 10 year treasury securities data. 
plot(tcm10y,xlab="Date", ylab="Treasury Yield ")

adf.test(tcm10y)
plot(diff(tcm10y))

dt10 <- diff(tcm10y)

adf.test(dt10)
adf.test(diff(tcm10y))

tsdisplay(diff(tcm10y))

Box.test(diff(tcm10y),lag = 5)

auto.arima(dt10)

m0 <- Arima(tcm10y, c(0,1,0))
tsdisplay(m0$residuals)

m1 <- auto.arima(tcm10y,d = 1,stepwise = FALSE)
summary(m1)
tsdisplay(m1$residuals)

Box.test(m1$residuals,5)

m1a <- Arima(tcm10y, order = c(0,1,2))
summary(m1a)

forecast(m1a,h = 5)

plot(forecast(m1,h=5),include = 50)

accuracy(m1)


