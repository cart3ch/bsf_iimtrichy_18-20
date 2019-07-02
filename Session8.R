## Pre-class assignment
data<-read.csv("rdata/Machine_Breakdowns.csv")
model<-lm(data$Cost.of.Repairs ~  data$Age.of.machine + data$Machine, data = data)
# model with interaction effects
model<-lm(data$Cost.of.Repairs ~  data$Age.of.machine + data$Age.of.machine:data$Machine, data = data)
summary(model)
           

#One-way anova test
means.data<-data.frame(age=data$Age.of.machine, machine=data$Machine)
library(dplyr)
group_by(means.data, machine) %>%
  summarise(
    count = n(),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(means.data, x = "machine", y = "age", 
          color = "machine", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = levels(means.data$machine),
          ylab = "Age", xlab = "Machine")

res.aov <- aov(age ~ machine, data = means.data)
summary(res.aov)
#since P-value is low, reject the null hypothesis of equal means


#### Class Session
