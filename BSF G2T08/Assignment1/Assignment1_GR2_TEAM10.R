#######################################################################################

                              #BSF Group 2 Team No.10
#Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122

#######################################################################################

### Seting up the correct directory
#setwd("~/PGP-iimt/Term4/BSF gr2/bsf_iimtrichy_18-20/BSF G2T08/Assignment1/") #Windows
#setwd("~/rwork/rprojects/bsf_iimtrichy_18-20/BSF G2T08/Assignment1/") #Linux

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
quest1<-housing[,1:3]
quest1

### Ques 2 - Mean price of 4-bedroom segment
quest2<-aggregate(housing$price, by=list(housing$bedrooms), FUN= "mean")
quest2[4,]

### Quest 3 - Summary of price per lotsize
quest3<-housing$price/housing$lotsize
summary(quest3)

### Quest 4 - stories.summary dataframe with no. of houses, mean house price, median house price, maximum house price
quest4<-aggregate(housing$price, by=list(housing$stories), FUN = function(x)
  c(count=length(x), avg=mean(x),median=median(x),max=max(x)))
quest4
str(quest4) #NOT ABLE TO UNDERSTAND THE STRUCTURE OF THIS OBJECT

### Quest 5 - How many houses have lotsizes between 4000 and 6000
quest5<- subset(housing, lotsize >= 4000 & lotsize <= 6000) # Note the difference between && (comparison of vectors) and & (element wise comparison)
quest5
length(quest5$lotsize)

### Quest 6 - Order houses by highest lotsizes and print first 10 houses
quest6<-housing[rev(order(housing$lotsize)),]
quest6[1:10,]

### Quest 7 - Find the coefficient of variation of the house price with and without aircon
quest7<-aggregate(housing$price, by= list(housing$airco), FUN = function(x)
  c(count=length(x), avg=mean(x), sd=sd(x), cov=sd(x)/mean(x)))
quest7

### Quest 8 - Boxplot of house prices with preferred and non-preferred area
quest8<-table


### Quest 9 - 
