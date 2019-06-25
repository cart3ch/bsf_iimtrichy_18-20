
library(readxl)

##Setting the working directory
setwd("~/PGP-iimt/Term4/BSF gr2/bsf_iimtrichy_18-20/BSF G2T10/Assignment1/")

#Read excel file
house<-read_excel("Housing.xlsx")

##Simple linear regression
m1_house<-lm(price~lotsize, data=house)
summary(m1_house)
plot(price~lotsize, xlab="Lot Size", data= house, ylab="House Price")
abline(m1_house,col="blue")

##Multiple linear regression
m2_house<-lm(price~lotsize + bedrooms+bathrms+stories, data=house)
summary(m2_house)
str(m2_house)

##Analyzing regression output
coefficients(m2_house)
m2_house$coefficients
head(residuals(m2_house),5)

## Prediction: Existing data
head(predict(m2_house))



## Prediction for a new observation
predict(m2_house,newdata = 
          data.frame(lotsize=1000, bedrooms=2,
                     bathrms=2, stories=5))
##In-class activity
south_data<-data.frame(expense=c(8,8,8,8,8,8,8,19,8,8,8), sales= c(65.8,57.6,77.1,88.4,84.7,70.4,52.5,125,55.6,79.1,68.9))
summary(south_data)
model<-lm(sales~expense, data=south_data)
summary(model)
plot(model)
predict(model,newdata = data.frame(expense=15))
plot(sales~expense, xlab="Expense", data= south_data, ylab="Sales")
abline(model,col="blue")
