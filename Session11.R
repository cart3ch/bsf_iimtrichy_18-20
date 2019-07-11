## In-class session

### Time-series analysis

library(readxl)
tsdata<-read_excel("rdata/Tea Production_Classical_Decomposition.xlsx")


#creating a time series object in R
pdnts <- ts(tsdata$Production, start = c(1981,1), frequency = 12)

m1 <- decompose(pdnts,type = "a")
plot(m1)

library(forecast)
#### EXPONENTIAL SMOOTHING

sm1 <- ses(pdnts) 
summary(sm1)

sm2 <- holt(pdnts) 
summary(sm2)

sm3 <- hw(pdnts) 
summary(sm3)

accuracy(sm1)
accuracy(sm2)
accuracy(sm3)



sm5 <- ets(pdnts)
summary(sm5)

tsdata$Year1<-seq(from=1980, by= 1/12, length.out = 120)
tran.pdn <- subset(tsdata, Year < 1989 )

lm1 <- lm(data=tsdata, Production~Year+Month)
summary(lm1)

lm2 <- lm(data=tsdata, Production~Year1+Month)
summary(lm2)

test.pdn <- subset(tsdata, Year >= 1989 )
pred <- predict(lm1, newdata = test.pdn)

sqrt( mean(( test.pdn$Production - pred)^2 ))


train.ts <- window(pdnts, end=c(1988,12))
test.ts <- window(pdnts, start=c(1989,1))

sm3 <- hw(train.ts)
accuracy(sm3,test.ts)



sm4 <- hw(train.ts,seasonal = "m")
accuracy(sm4,test.ts)


