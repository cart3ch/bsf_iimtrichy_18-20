#######################################################################################
# BSF Group 2 Team No.10
# Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122, Raju 1801166
# Assignment 1
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
predict(m1.logit)
logit.value<-predict(m1.logit,newdata = data.frame(Mortgage=1700,Famsize=3),type="response")
logit.value
#Normal CDF
m1.probit <- glm(data=mort, resp~Mortgage+Famsize,  family = binomial(link = "probit"))
summary(m1.probit)
exp(m1.probit$coefficients)
predict(m1.probit)
probit.value<-predict(m1.probit,newdata = data.frame(Mortgage=1700,Famsize=3),type="response")
probit.value


### Question 2
mort2 <- read.csv("rdata/sagedat2.csv",stringsAsFactors = FALSE)
mort2$resp[mort$takeoffer == "take offer"] <- 0
mort2$resp[mort$takeoffer == "decline offer"] <- 1
m2.logit <- glm(data=mort2, resp~Mortgage+Famsize, family=binomial(link = "logit"))
summary(m2.logit)
exp(m2.logit$coefficients)
predict(m2.logit)
logit2.value<-predict(m2.logit,newdata = data.frame(Mortgage=1700,Famsize=3),type="response")
logit2.value


### Question 3(a)
german<-read.table("rdata/german.data",header = FALSE)
str(german)
View(german)
german[,6:20]<-NULL
names(german)<-c("Account Status","Duration in month","Credit history","Purpose","Credit Amount","Response")
### Question 3(b)



### Question 3(c)