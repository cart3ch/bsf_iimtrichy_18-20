#### Reading the data
setwd("BSF G2T10/Case/")
library(readxl)
marriot<-read_excel("Marriot_data (1).xlsx")
View(marriot)

#### Data Cleaning
marriot$`DOW INDICATOR1`<-factor(marriot$`DOW INDICATOR1`)
DOW<-c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday")
levels(marriot$`DOW INDICATOR1`)<-DOW
marriot$WEEK<-factor(marriot$WEEK)
levels(marriot$WEEK)<-c("Week 1","Week 2","Week 3","Week 4","Week 5","Week 6","Week 7",
                        "Week 8","Week 9","Week 10","Week 11","Week 12","Week 13","Week 14")
marriot[,c(7,8,9,10,11)]<-NULL
data.subset<-data.frame(Demand= marriot$DEMAND[1:87], Day = marriot$`DOW INDICATOR1`[1:87], Week = marriot$WEEK[1:87])
str(data.subset)


##Univariate Analysis
summary(data.subset$Demand)

demand.day<-ggplot(data.subset,aes(x = data.subset$Day ,
                                   y = data.subset$Demand,
                                   fill=data.subset$Day))+geom_boxplot()
demand.day+labs(title = "Demand Variation Across Days",
                x="Day of Week", y="No. Of Rooms Demanded", 
                fill="Days of the Week")+
                theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5))

demand.week<-ggplot(data.subset,aes(x = data.subset$Week,
                                    y = data.subset$Demand,
                                    fill=data.subset$Week))+geom_boxplot()
demand.week+labs(title = "Demand Variation Across Weeks",
                x="Week No.", y="No. Of Rooms Demanded", 
                fill="Week No.")+
                theme(axis.text.x = element_blank(),plot.title = element_text(hjust = 0.5))


### Creating a time-series object
demand.ts<-ts(data.subset$Demand,frequency = 7)
autoplot(demand.ts, ylab = "Total no. of rooms demamded", xlab = "Week no.",
         main ="Marriot Hotel Rooms Demanded : May 23 - Aug 18,1987 ", xlim = c())+
         theme(plot.title = element_text(hjust = 0.5))

ggsubseriesplot(demand.ts) +
  ylab("No. Rooms Demanded") +
  ggtitle("Hotel Room Demand Variation")+theme(plot.title = element_text(hjust = 0.5))

ggseasonplot(demand.ts, year.labels=TRUE, year.labels.left=TRUE) +  ylab("No. of rooms demanded") +
  ggtitle("Seasonal plot: Room Demand variation")+theme(plot.title = element_text(hjust = 0.5))


### Forecasting Procedure

library(fma)
library(forecast)

##Staionarity Tests
library(fUnitRoots)
adfTest(demand.ts) #  #p-value> 0.05 : Fail to reject null hypothesis = Non-stationarity
library(tseries)
kpss.test(demand.ts) #p-value> 0.05 : Fail to reject null hypothesis = Stationarity

##Trend-Seasonality Decompositions
model.ets<-ets(demand.ts)
model.ets
plot(model.ets)
checkresiduals(model.ets$residuals)
model.ets$aic
accuracy(model.ets)

#Naive
model.naive<-naive(demand.ts)
model.naive
checkresiduals(model.naive$residuals)
accuracy(model.naive)

#Auto-ARIMA
model.arima<-auto.arima(demand.ts)
model.arima
checkresiduals(model.arima$residuals)
model.arima$aic
accuracy(model.arima)


autoplot(demand.ts) +
  autolayer(fitted(model.ets), series = "ETS") +
  autolayer(fitted(model.arima), series = "Auto-ARIMA") +
  autolayer(fitted(model.naive), series = "Naive")
  xlab("Week No.") + ylab("Demand for Hotel Rooms") +
  ggtitle("Marriot Hotel Room Demand Forecasting") +
  guides(colour = guide_legend(title = " "))

## Model Comparison

## Forcast
m1<-forecast(model.naive, h=5);m1
m2<-forecast(model.ets, h=5);m2
m3<-forecast(model.arima, h=5);m3

