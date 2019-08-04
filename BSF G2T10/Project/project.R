#######################################################################################
# BSF Group 2 Team No.10
# Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122, Raju 1801166
# Project : Logistic Regression
#######################################################################################

### Seting up the correct directory
setwd("BSF G2T10/Project/")

### Data Cleaning
#win.train<-read.csv("dota2Train_win.csv")
win.test<-read.csv("dota2Test_win.csv")

#lose.train<-read.csv("dota2Train_lose.csv")
#lose.test<-read.csv("dota2Test_lose.csv")

table(win.test$result)
head(win.test,2)

plot(table(win.test$result)
table(Affairs$resp)


m1.logit <- glm(data=win.test, result~., family=binomial)
summary(m1.logit)
exp(m1.logit$coefficients)
