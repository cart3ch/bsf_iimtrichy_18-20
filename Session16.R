### Pre-Class
mort <- read.csv("rdata/sagedat2.csv",stringsAsFactors = FALSE)
mort$resp[mort$takeoffer == "take offer"] <- 1
mort$resp[mort$takeoffer == "decline offer"] <- 0
m2.logit <- glm(data=mort, resp~Mortgage+Famsize, family=binomial())
summary(m2.logit)
exp(m2.logit$coefficients)
predict(m2.logit)
val<-predict(m2.logit,newdata = data.frame(Mortgage=1700,Famsize=3),type="response")
val

### In-Class Handson
library(readxl)

bank <- read_excel("rdata/Bank_Data.xlsx",)

bank$ZIP_Code <- NULL
table(bank$Personal_Loan)

str(bank$Education)

# Treat Education as a categorical variable
bank$Education <- factor(bank$Education, levels = c(1,2,3),
                         labels = c("Undergrad", "Graduate", "Advanced/Professional"))

str(bank$Education)
table(bank$Education)


# Partition data
set.seed(2)
train.index <- sample(1:nrow(bank), nrow(bank)*.6)

train.bank <- bank[train.index,]
test.bank <- bank[-train.index,]

fullmodel <- glm(data=train.bank, Personal_Loan~., family=binomial)

summary(fullmodel)

zeromodel <- glm(data=train.bank, Personal_Loan~1, family=binomial)
summary(zeromodel)

library(MASS)
stepmodel <-  step(zeromodel, list(lower=formula(zeromodel),
                                   upper=formula(fullmodel)),
                   direction="both",trace=0)

summary(stepmodel)


m1 <- glm(formula = Personal_Loan ~ Income + Education + CD_Account +
            Family + CreditCard + CCAvg + Online + Securities_Account +
            Age , family = binomial, data = train.bank)

summary(m1)


m2 <- glm(formula = Personal_Loan ~ Income + Education + CD_Account +
            Family + CreditCard + CCAvg + Online + Securities_Account  , family = binomial, data = train.bank)

summary(m2)



m3 <- glm(formula = Personal_Loan ~ Income + Education + CD_Account +
            Family + CreditCard + CCAvg + Online,
          family = binomial, data = train.bank)

summary(m3)


library(ggplot2)
library(lattice)
library(caret)


library(pscl)
library(ResourceSelection)
library(DescTools)
pR2(m3)
hoslem.test(train.bank$Personal_Loan, fitted(m3), g=3)

PseudoR2(m3,which = c("Nagelkerke","McFadden", "Nagel"))

HosmerLemeshowTest(prob, test.bank$Personal_Loan,
                   ngr = 10,  verbose = FALSE)

train.bank$prob <- predict(m3,type="r")
CUTOFF <- quantile(train.bank$prob,.9)
train.bank$pred <- ifelse(train.bank$prob > CUTOFF,1,0)

table(train.bank$pred, train.bank$Personal_Loan)
Conf(x = train.bank$pred,ref=train.bank$Personal_Loan)

test.bank$pred <- ifelse(predict(m3, test.bank)>CUTOFF,1,0)
table(test.bank$pred, test.bank$Personal_Loan)

Conf(x = test.bank$pred,ref=test.bank$Personal_Loan)


##  Do the Lift and Gain Chart in Excel


