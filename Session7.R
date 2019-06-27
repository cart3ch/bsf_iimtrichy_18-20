## Pre-clas
soap_sud<-read.csv("dat.csv")
sud_model<-lm(suds~soap, data=soap_sud)
summary(sud_model)
plot(suds~soap, xlab="Soap (in gms)", data= soap_sud, ylab="Sud height")
abline(sud_model,col="blue")
anova(sud_model)

## In-class Session
## Multiple Linear Regression
m3_house <- lm(price ~ lotsize+ bedrooms+ prefarea + stories, data=house)
summary(m3_house)



library(car)
qqPlot(m3_house)


crPlots(m3_house)

ncvTest(m3_house)

influencePlot(m3_house)

vif(m3_house)
