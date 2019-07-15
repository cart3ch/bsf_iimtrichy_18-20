#######################################################################################
# BSF Group 2 Team No.10
# Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122, Raju 1801166
# Assignment 1
#######################################################################################

### Seting up the correct directory
setwd("BSF G2T10/Assignment2/")

### Reading Excel
library("readxl")
ipl.data <- read_xlsx("IMB381IPL2013.xlsx",sheet = 3)
View(ipl.data)
str(ipl.data)
ipl.data$AGE<-factor(ipl.data$AGE)
levels(ipl.data$AGE)<-c(" -less than 25 yrs", " -between 25-35 yrs", " -more Than 35 yrs")
ipl.data$COUNTRY<-factor(ipl.data$COUNTRY)
ipl.data$TEAM<-factor(ipl.data$TEAM)
ipl.data$`PLAYING ROLE`<-factor(ipl.data$`PLAYING ROLE`)
ipl.data$`CAPTAINCY EXP`<-factor(ipl.data$`CAPTAINCY EXP`)
levels(ipl.data$`CAPTAINCY EXP`)<-c(" -no", " -yes")
str(ipl.data)

##Question 1
price.age.model <- lm(`SOLD PRICE` ~ AGE -1 , data=ipl.data) 
summary(price.age.model)
anova(price.age.model)


library(ggplot2)
plot <- ggplot(data=ipl.data, aes(x=AGE, y=`SOLD PRICE`, colour=factor(AGE)))
plot + stat_smooth(method=lm, fullrange=FALSE) + geom_point()

##Question 2
data.slice<-subset(ipl.data,`PLAYING ROLE`=="Batsman")
model2<-lm(`SOLD PRICE`~`SR -B` + `CAPTAINCY EXP` + `SR -B`:`CAPTAINCY EXP`, data = data.slice)
summary(model2)
anova(model2)
plot <- ggplot(data=ipl.data, aes(x=`SR -B`, y=`SOLD PRICE`, colour=factor(`CAPTAINCY EXP`)))
plot + stat_smooth(method=lm, fullrange=FALSE) + geom_point()

##Question 3
model3<-lm(`SOLD PRICE`~`AVE` , data = ipl.data)
summary(model3)
      ## Data Visualization        
      ##plot <- ggplot(data=ipl.data, aes(x=`AVE`, y=`SOLD PRICE` ))
      ##plot + stat_smooth(method=lm, fullrange=FALSE) + geom_point()

##Question 4
model4<-lm(`SOLD PRICE`~`AVE`+`SIXERS` , data = ipl.data)
summary(model4)

##Question 5
library(leaps)
leaps<-regsubsets(`SOLD PRICE`~ `AGE`+  `PLAYING ROLE`+ `T-RUNS`+ `T-WKTS`+ `ODI-RUNS-S`+ `ODI-SR-B`+ `ODI-WKTS`+ 
                     `ODI-SR-BL`+ `CAPTAINCY EXP`+ `RUNS-S`+ `HS`+ `AVE`+ `SR -B`+ `SIXERS`+ `RUNS-C`+ `WKTS`+ `AVE-BL`+ 
                     `ECON`+ `SR-BL`+ `BASE PRICE`, data = ipl.data, nbest = 1)
plot(leaps, scale="adjr2")

#Ideal Model
full.model<-lm(`SOLD PRICE`~ `AGE`+  `PLAYING ROLE`+ `T-RUNS`+ `T-WKTS`+ `ODI-RUNS-S`+ `ODI-SR-B`+ `ODI-WKTS`+ 
                 `ODI-SR-BL`+ `CAPTAINCY EXP`+ `RUNS-S`+ `HS`+ `AVE`+ `SR -B`+ `SIXERS`+ `RUNS-C`+ `WKTS`+ `AVE-BL`+ 
                 `ECON`+ `SR-BL`+ `BASE PRICE`, data = ipl.data)

ideal<-lm(`SOLD PRICE`~ `AGE`+  `T-RUNS`+ `ODI-RUNS-S`+ `RUNS-S`+
          `HS`+ `RUNS-C`+ `BASE PRICE`, data = ipl.data)
summary(full.model)
summary(ideal)
anova(ideal)
anova(full.model)

#Manually calculating the Mallow's Cp 
SSE<-sum(ideal$residuals**2) # for the ideal model
MSE<-7.7746e+10  # Taken from the full.model annova table
n<-nrow(ipl.data)
p<-9; p#no. of parameters in our ideal model
Cp<-(SSE/MSE)-(n-(2*p)); Cp

library(carData)
library(car)
subsets(leaps, statistic="cp",
        main="Mallows Cp Distance Plot All Subsets Regression")
abline(1,1,lty=2,col="blue")

#To Find outliers in our model
library(car)
influencePlot(ideal, id.method = "identify", main="Influence Plot", sub="Circle size is proportional to Cook's distance")
#From the graph we identify the outliers 
outlier.pts<-ipl.data[c(84,24,94,112,114),];outlier.pts
outlier.X<-data.frame()
predict(outlier.pts,ideal)

