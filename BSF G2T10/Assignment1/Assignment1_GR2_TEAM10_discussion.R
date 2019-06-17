#######################################################################################

                              #BSF Group 2 Team No.10
#Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122

#######################################################################################

### Seting up the correct directory
#setwd("~/PGP-iimt/Term4/BSF gr2/bsf_iimtrichy_18-20/BSF G2T10/Assignment1/") #Windows
setwd("~/rwork/rprojects/bsf_iimtrichy_18-20/BSF G2T10/Assignment1/") #Linux

### Reading Excel file
library(readxl)
raw_data<-read_excel("Housing.xlsx",col_names = TRUE)
raw_data # This the table with raw data
str(raw_data) #All strings are considered as characters
        housing<-data.frame(raw_data,stringsAsFactors = TRUE)
        str(housing)  #QUESTION WHY ARE THE CHARACTER ROWS NOT TURNING INTO FACTORS. 
        housing$driveway<-as.factor(housing$driveway)
        housing$recroom<-as.factor(housing$recroom)
        housing$fullbase<-as.factor(housing$fullbase)
        housing$gashw<-as.factor(housing$gashw)
        housing$airco<-as.factor(housing$airco)
        housing$prefarea<-as.factor(housing$prefarea)
        str(housing)        
        

### Quest 1 - Return a dataframe with columns price, lotsize, bedrooms
quest1<-data.frame(price=housing$price, lotsize=housing$lotsize, bedrooms=housing$bedrooms)
(quest1)

### Ques 2 - Mean price of 4-bedroom segment
quest2 <- subset(housing, bedrooms==4)
mean(quest2$price)

### Quest 3 - Summary of price per lotsize
quest3<-housing$price/housing$lotsize
summary(quest3)

### Quest 4 - stories.summary dataframe with no. of houses, mean house price, median house price, maximum house price
stories.summary<-aggregate(housing$price, by=list(housing$stories), FUN = function(x)
  c(count=length(x), avg=mean(x),median=median(x),max=max(x)))
stories.summary
str(stories.summary) #NOT ABLE TO UNDERSTAND THE STRUCTURE OF THIS OBJECT

### Quest 5 - How many houses have lotsizes between 4000 and 6000
quest5<- subset(housing, lotsize >= 4000 & lotsize <= 6000) # Note the difference between && (comparison of vectors) and & (element wise comparison)
length(quest5$lotsize)

### Quest 6 - Order houses by highest lotsizes and print first 10 houses
quest6<-housing[rev(order(housing$lotsize)),]
quest6[1:10,2]

### Quest 7 - Find the coefficient of variation of the house price with and without aircon
quest7<-aggregate(housing$price, by= list(housing$airco), FUN = function(x)
  c(cov=sd(x)/mean(x)))
quest7

### Quest 8 - Boxplot of house prices with preferred and non-preferred area
vec1<-housing$price[housing$prefarea=='yes']; vec1
vec2<-housing$price[housing$prefarea=='no']; vec2
boxplot(vec1,vec2)

boxplot(housing$price[housing$prefarea=='yes'], housing$price[housing$prefarea=='no'])


### Quest 9 - Scatterplot of price vs lotsize

plot(housing$price, housing$lotsize, xlab = "Price", ylab = "Lot Size")
cor(housing$price,housing$lotsize)

### Quest 10 - Does the relationship between price and lotsize same for all stories segments?

cor(housing$price[housing$stories==1],housing$lotsize[housing$stories==1])
cor(housing$price[housing$stories==2],housing$lotsize[housing$stories==2])
cor(housing$price[housing$stories==3],housing$lotsize[housing$stories==3])
cor(housing$price[housing$stories==4],housing$lotsize[housing$stories==4])

par(mfrow=c(2,2))
plot(housing$price[housing$stories==1],housing$lotsize[housing$stories==1], xlab = "Price", ylab = "Lot Size", main="One storey house")
plot(housing$price[housing$stories==2],housing$lotsize[housing$stories==2], xlab = "Price", ylab = "Lot Size", main="Two storey house")
plot(housing$price[housing$stories==3],housing$lotsize[housing$stories==3], xlab = "Price", ylab = "Lot Size", main="Three storey house")
plot(housing$price[housing$stories==4],housing$lotsize[housing$stories==4], xlab = "Price", ylab = "Lot Size", main="Four storey house")

