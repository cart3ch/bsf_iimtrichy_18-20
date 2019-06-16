#######################################################################################
# BSF Group 2 Team No.10
# Members : Paras 1801035, Karthigeyan 1801093, Shanmugapriya 1801115, Niranjan 1801122
# Assignment 1
#######################################################################################

### Seting up the correct directory
setwd("F:/Trichy/Acads/Term 4/BSF/Assignment/")

### Reading Excel
install.packages("readxl")
library("readxl")
housing <- read_xlsx("Housing.xlsx")
View(housing)

### Quest 1 - Return a dataframe with columns price, lotsize, bedrooms
quest1 <- data.frame(price=housing$price, lotsize=housing$lotsize, bedrooms=housing$bedrooms)
quest1

### Ques 2 - Mean price of 4-bedroom segment
price_4beds <- subset(housing, bedrooms==4) #create a subset with houses having 4 bedrooms
mean(price_4beds$price) #calculate mean price for all houses in the subset

### Quest 3 - Summary of price per lotsize
price_per_lotsize <- housing$price/housing$lotsize
summary(price_per_lotsize)

### Quest 4 - stories.summary dataframe with no. of houses, mean house price, median house price, maximum house price
# Create an aggregate function listed by number of stories. Run 4 required functions in each category - no of houses, mean price, median price and maximum price.
stories.summary <- aggregate( housing$price, by=list(housing$stories), FUN = function(x) c(count=length(x), avg=mean(x),median=median(x),max=max(x)))
stories.summary

### Quest 5 - How many houses have lotsizes between 4000 and 6000
lot_bn_4k_6k <- subset(housing, lotsize >= 4000 & lotsize <= 6000) #create a subset which is intersection of houses having more than 4000 lotsize and less thna 6000 lotsize
length(lot_bn_4k_6k$lotsize) #calculate the number of rows in the subset

### Quest 6 - Order houses by highest lotsizes and print first 10 houses
highest_10_lots <- housing[rev(order(housing$lotsize)),] #sort the housing data by lotsize in descending order
highest_10_lots[1:10,2] #return first 10 rows and data from only 2nd column

### Quest 7 - Find the coefficient of variation of the house price with and without aircon
coeff_variation <- aggregate(housing$price, by= list(housing$airco), FUN = function(x) c(cov=sd(x)/mean(x)))
coeff_variation

### Quest 8 - Boxplot of house prices with preferred and non-preferred area

boxplot(housing$price[housing$prefarea=='yes'], housing$price[housing$prefarea=='no'],names = c('Preferred Area','Non-preferred Area'))

### Quest 9 - Scatterplot of price vs lotsize
plot(housing$price, housing$lotsize, xlab = "Price", ylab = "Lot size")
cor(housing$price,housing$lotsize)

### Quest 10 - Does the relationship between price and lotsize same for all stories segments?

par(mfrow=c(2,2))
plot(housing$price[housing$stories==1],housing$lotsize[housing$stories==1], xlab = "Price", ylab = "Lot Size", main="One storey house")
plot(housing$price[housing$stories==2],housing$lotsize[housing$stories==2], xlab = "Price", ylab = "Lot Size", main="Two storey house")
plot(housing$price[housing$stories==3],housing$lotsize[housing$stories==3], xlab = "Price", ylab = "Lot Size", main="Three storey house")
plot(housing$price[housing$stories==4],housing$lotsize[housing$stories==4], xlab = "Price", ylab = "Lot Size", main="Four storey house")

