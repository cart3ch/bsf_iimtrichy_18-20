# Session 15
######################################################
library(AER)
data(Affairs)
help("Affairs")

table(Affairs$affairs)
Affairs$resp <- ifelse(Affairs$affairs > 0 ,1,0)
head(Affairs,2)

plot(table(Affairs$children,Affairs$resp))
table(Affairs$resp)

m1.logit <- glm(data=Affairs, resp~children+rating, family=binomial)
summary(m1.logit)
exp(m1.logit$coefficients)

predict(m1.logit)
predict(m1.logit,newdata = data.frame(children="yes",rating=4))
predict(m1.logit,newdata = data.frame(children="yes",rating=4),
        type="response")



Affairs$prob <- predict(m1.logit,type="response")

Affairs$pred_resp <- ifelse(Affairs$prob >quantile(Affairs$prob,.75), 1,0)
table(Affairs$resp,Affairs$pred_resp)
plot(table(Affairs$resp,Affairs$pred_resp))




