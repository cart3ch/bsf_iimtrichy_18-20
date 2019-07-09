### Time-series analysis

library(readxl)
tsdata<-read_excel("rdata/Tea Production_Classical_Decomposition.xlsx")



#creating a time series object in R
pdnts <- ts(tsdata$Production, start = c(1981,1), frequency = 12)

### Plots
library(fma)

plot(pdnts)
points(pdnts)
seasonplot(pdnts,col=rainbow(12), year.labels=TRUE)
monthplot(pdnts)



#additive model
ts_add <- decompose(pdnts, type = "a")
str(ts_add)

plot(ts_add)


library(forecast)
train.ts <- window(pdnts, end=c(1989,12))
test.ts <- window(pdnts, start=c(1990,1))

m1 <- meanf(train.ts,h = 12)
m2 <- naive(train.ts,h = 12)
m3 <- snaive(train.ts,h = 12)

accuracy(m1,test.ts)
accuracy(m2,test.ts)
accuracy(m3,test.ts)



#lookingup for trend, seasonality and error values
m1.ad$trend
m1.ad$seasonal
m1.ad$random



#multiplicative model

ts_mult <- decompose(pdnts, type = "m")
str(ts_mult)

plot(ts_mult)


### STL Model
fit1 <- stl(pdnts, s.window="periodic")

plot(fit1)



#### EXPONENTIAL SMOOTHING

sm1 <- ses(pdnts, alpha = 0.2) 
summary(sm1)

sm2 <- holt(pdnts) 
summary(sm2)

sm3 <- hw(pdnts) 
summary(sm3)

accuracy(sm1)
accuracy(sm2)
accuracy(sm3)
