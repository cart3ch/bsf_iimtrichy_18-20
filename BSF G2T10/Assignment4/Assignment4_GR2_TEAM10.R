#######################################################################################
# BSF Group 2 Team No.10
# Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122, Raju 1801166
# Assignment 4
#######################################################################################

## Preparing the data 
mort <- read.csv("rdata/sagedat2.csv",stringsAsFactors = FALSE)
mort$resp[mort$takeoffer == "take offer"] <- 1
mort$resp[mort$takeoffer == "decline offer"] <- 0

### Question 1
#Logistic CDF
m1.logit <- glm(data=mort, resp~Mortgage+Famsize, family=binomial(link = "logit"))
summary(m1.logit)
exp(m1.logit$coefficients)
#Probit CDF
m1.probit <- glm(data=mort, resp~Mortgage+Famsize,  family = binomial(link = "probit"))
summary(m1.probit)
exp(m1.probit$coefficients)
# Comparing logit and probit
result<-data.frame("x"=seq(1:nrow(mort)),
                   "logit.prob"=sort(predict(m1.logit, type="r"),decreasing=TRUE), 
                   "probit.prob"=sort(predict(m1.probit, type="r"),decreasing=TRUE),
                   "logit.greater"= (predict(m1.logit, type = "r")>predict(m1.probit, type = "r")))


library(ggplot2)
p<-ggplot(data = result, aes(x)) + 
  geom_line(aes(y = logit.prob, colour = "Logit link")) + 
  geom_line(aes(y = probit.prob, colour = "Probit link"))
p + ggtitle("Comparing Logit and Probit links") +
  xlab("No. of Trials") + ylab("Probability")  


### Question 2
mort2 <- read.csv("rdata/sagedat2.csv",stringsAsFactors = FALSE)
mort2$resp[mort$takeoffer == "take offer"] <- 0
mort2$resp[mort$takeoffer == "decline offer"] <- 1
m2.logit <- glm(data=mort2, resp~Mortgage+Famsize, family=binomial(link = "logit"))
summary(m2.logit)
exp(m2.logit$coefficients)
predict(m2.logit,type = "r")
logit2.value<-predict(m2.logit,newdata = data.frame(Mortgage=1700,Famsize=3),type="response")
logit2.value
  

### Data preparation for question 3
german<-read.table("rdata/german.data",header = FALSE)
str(german)
View(german)
german[,6:20]<-NULL
names(german)<-c("Account Status","Duration in month","Credit history","Purpose","Credit Amount","Response")
german$Response[german$Response == 2] <- 0 #Bad
german$Response[german$Response == 1] <- 1 #Good
levels(german$`Account Status`)<-c("< 0 DM","Between 0-200 DM",">= 200 DM","no checking")
levels(german$`Credit history`)<-c("no credits taken","all paid back duly","existing paid back duly",
                                   "delay in paying","critical account")
levels(german$Purpose)<-c("new car","used car","others","furniture","radio/television","domestic appliances",
                          "repairs","education","retraining","business")





### Question 3(a)
table(german$Response)
t1 <-table(german$Response,german$`Account Status`) 
r1 <-plot(t1, col=rainbow(4), las=1)
t2 <-table(german$Response,german$`Credit history`) 
r2 <-plot(t2, col=rainbow(5), las=1)
t3 <-table(german$Response,german$Purpose) 
r3 <-plot(t3, col=rainbow(10), las=1)


### Question 3(b)
train.index <- sample(1:nrow(german), nrow(german)*.7)
train.german <- german[train.index,]
test.german <- german[-train.index,]


fullmodel <- glm(data=train.german, Response~., family=binomial(link = "logit"))
summary(fullmodel)

zeromodel <- glm(data=train.german, Response~1, family=binomial(link = "logit"))
summary(zeromodel)

library(MASS)
stepmodel <-  step(zeromodel, list(lower=formula(zeromodel),
                                   upper=formula(fullmodel)),
                   direction="both",trace=0)

summary(stepmodel)

m3<-glm(formula = Response ~ `Account Status` + `Duration in month` + 
             `Credit history` + Purpose, family = binomial(link = "logit"), 
           data = train.german)

### Question 3(c)
library(pROC)
library(DescTools)
train.german$prob <- predict(m3,type="r")
CUTOFF <- quantile(train.german$prob,.65)

train.german$pred <- ifelse(train.german$prob > CUTOFF,1,0)
table(train.german$pred, train.german$Response)
Conf(x = train.german$pred,ref=train.german$Response)

test.german$pred <- ifelse(predict(m3, test.german)>CUTOFF,1,0)
table(test.german$pred, test.german$Response)
Conf(x = test.german$pred,ref=test.german$Response)

#ROC chart
train.german$prob <- predict(m3,type="r")
plot(roc(train.german$Response, train.german$prob, direction="<"),
     col="blue", lwd=3, main="Tradeoff")
test.german$prob <- predict(m3,test.german, type="r")
plot(roc(test.german$Response, test.german$prob, direction="<"),
     col="red", lwd=3, main="Tradeoff")

# Excel Sheet for lift and gain chart
library(xlsx)
write.xlsx(train.german, "rdata/traingerman.xlsx")
