#######################################################################################
# BSF Group 2 Team No.10
# Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122, Raju 1801166
# Assignment 1
#######################################################################################

### Seting up the correct directory
setwd("BSF G2T10/Assignment3/")


### Question 1 

##Simulating and AR(1) process

# initialize seed for random numbers
set.seed(999) 
C<-100  
phi_1<-0.8 
y<-1:108 
y[1]<-C/(1-phi_1); y[1]

for (i in 2:108 ) {
  y[i]<-(phi_1*y[i-1])+C+rnorm(1, mean = 0, sd = 1) #Random walk equation
  n<-n+1
}

train<-ts(y[1:100],frequency = 1) # creating a training dataset from Y
test<-ts(y[100:108],frequency = 1) # creating a test datatset from Y

# Plotting the Random Walk for Training
autoplot(train , ylab = "Y", xlab = "n", main ="AR(1) Process Illustration")

## Estimating AR(1) Model
model<-arima(train,order = c(1,0,0))
prediction<-forecast(model,h=8)
prediction
test
# Plotting the Predictions
autoplot(prediction, ylab = "Y", xlab = "n", main ="AR(1) Prediction")

#Comparing the preditions with the test data
RMSE= sqrt(mean((prediction$mean-y[101:108])^2))
#Root mean square error
RMSE
#Residual Check
checkresiduals(prediction$residuals)


### Question 2 - Developing and ARIMA process using "Modeling Procedure"

# We Start with the time series model
autoplot(WWWusage, ylab = "No. of Users connected to the server", xlab = "Minutes", main ="World wide web usage")
#Visual inspection does not give us any idea about stationarity
#Next we try to confirm if the data is stationary or not using the ADF test

##Staionarity Tests
library(tseries)
adf.test(WWWusage) #  #p-value> 0.05 : Fail to reject null hypothesis = Non-stationarity
##So the time series contains a random walk in it 


#Taking first differences
diff.series<-diff(WWWusage, lag = 1, differences = 1)
autoplot(diff.series, ylab = "No. of Users connected to the server", xlab = "Minutes", main ="WWWusage first differencing")
##Staionarity Tests
adf.test(diff.series) #  #p-value> 0.05 : Fail to reject null hypothesis = Non-stationarity

#Taking second differences
diff2.series<-diff(series, lag = 1, differences = 1)
autoplot(diff2.series, ylab = "No. of Users connected to the server", xlab = "Minutes", main ="WWWusage second differencing")
##Staionarity Tests
adf.test(diff2.series) #  #p-value< 0.05 : Reject null hypothesis = Non-stationarity
#Therefore the differencing parameter d=2

# To find the value of p, we plot the PACF of diff2.series
pacf(diff2.series)
#From the graph we see that a lag of 2 is significantly greater than zero. Hence p=2

# To find the value of q, we plot the ACF of diff2.series
acf(diff2.series)
#From the we see that a lag of 1 has a value significantly greater than zero

# Thus our ARIMA (p,d,q) model is is ARIMA (2,2,1)
fit1<-arima(WWWusage,order = c(2,2,1))
fit1
#The AIC score is 513.26
fit2<-auto.arima(WWWusage)
fit2
# The AIC score is 514.3
# ARIMA(2,2,1) has the lowest AIC value, hence we will select this model

#Now we check the residuals of our chosen model
checkresiduals(fit1$residuals)
#Now the residuals look like white noise and the ACF values are almost zero. Hence our models is correct.