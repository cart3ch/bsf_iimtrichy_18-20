#######################################################################################
# BSF Group 2 Team No.10
# Members : Paraskumar   1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122, Raju 1801166
# Project : Logistic Regression
#######################################################################################

### Data Cleaning
dota2<-read.csv("BSF G2T10/Project/dota2_final_v0.csv", colClasses = "factor")
dota2.hero<-dota2
dota2.hero$region<-NULL
str(dota2)
str(dota2.hero)
### Libraries 
library(pROC)
library(MASS)
library(ggplot2)
library(lattice)
library(caret)
library(pscl)
library(ResourceSelection)
library(DescTools)

## Data Splitting
set.seed(999)
train.index <- sample(1:nrow(dota2.hero), nrow(dota2.hero)*.8)
train.dota2 <- dota2.hero[train.index,]
test.dota2 <- dota2.hero[-train.index,]
table(train.dota2$result)
table(test.dota2$result)


### Modelling

## Step Model

fullmodel <- glm(data=train.dota2, result~., family=binomial)
summary(fullmodel)

zeromodel <- glm(data=train.dota2, result~1, family=binomial)
summary(zeromodel)

stepmodel <-  step(zeromodel, list(lower=formula(zeromodel),
                                   upper=formula(fullmodel)),
                   direction="both",trace=0)

summary(stepmodel)

m3<-glm(formula = result ~ find_furion + choose_skeleton_king + choose_furion + 
          find_oracle + choose_nevermore + choose_morphling + choose_shadow_demon + 
          choose_omniknight + choose_witch_doctor + choose_invoker + 
          choose_mirana + find_lion + choose_pudge + find_pudge + choose_earth_spirit + 
          choose_leshrac + choose_obsidian_destroyer + find_ogre_magi + 
          find_ancient_apparition + choose_wisp + choose_rattletrap + 
          find_faceless_void + choose_rubick + find_phantom_assassin + 
          choose_visage + find_weaver + choose_spectre + choose_clinkz + 
          find_omniknight + find_techies + find_riki + find_chaos_knight + 
          choose_lion + choose_disruptor + choose_bane + choose_luna + 
          find_spectre, family = binomial, data = train.dota2)

summary(m3)

## Confusion Matrix
#ROC chart
train.dota2$prob <- predict(m3,type="r")
plot(roc(train.dota2$result, train.dota2$prob, direction="<"),
     col="blue", lwd=3, main="Tradeoff")
test.dota2$prob <- predict(m3,test.dota2, type="r")
plot(roc(test.dota2$result, test.dota2$prob, direction="<"),
     col="red", lwd=3, main="Tradeoff")


#Training data
CUTOFF <- quantile(train.dota2$prob,.0.5)
train.dota2$pred <- ifelse(train.dota2$prob > CUTOFF,1,0)
table(train.dota2$pred, train.dota2$result)
Conf(x = train.dota2$pred,ref=train.dota2$result, pos = 1)
#Test data
test.dota2$prob<- predict(m3,test.dota2, type = 'r')
test.dota2$pred <- ifelse(predict(m3, test.dota2)>CUTOFF,1,0)
table(test.dota2$pred, test.dota2$result)
Conf(x = test.dota2$pred,ref= test.dota2$result, pos = 1)
plot(train.dota2$prob)



### Model Analysis
pR2(m3)
hoslem.test(train.dota2$result, fitted(m3))
PseudoR2(m3,which = c("Nagelkerke","McFadden", "Nagel"))
HosmerLemeshowTest(fitted(m3), train.dota2$result)

p<-ggplot(test.dota2, aes(x = prob)) + geom_histogram() +  
  ylab("No. of Instances") + xlab("Predicted Probabilites")
  ggtitle("Histogram Of Predicted Probabilities")+
    theme(plot.title = element_text(hjust = 0.5))
#hist(test.dota2$prob)

### Accuracy
mean(test.dota2$pred == test.dota2$result)

