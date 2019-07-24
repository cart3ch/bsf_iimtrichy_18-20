### Pre-class
hdata <- data.frame(y = c(1, 1, 1, 1, 0, 0, 0, 0), x = c(10, 9, 7, 6, 7, 5, 4, 1))
fit<-lm(y~x,data = hdata)
summary(fit)
plot(hdata$x,hdata$y)
predict(fit,newdata = data.frame(x=2))

install.packages("AER",dependencies = TRUE)

## in-class

library(AER)
