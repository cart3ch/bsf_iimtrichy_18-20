##### Pre-read assignment
oddeven <- function(number=2){ #equality here assigns a default value. Good practice to assign defaults for functions.
  if(number%%1 == 0){
    if(number%%2==0){print("EVEN")}
    else{print("ODD")}
  } else {print("NOT AN INTEGER")} #use else on the same line as the close braces of the if statement since R is an interpreter
}
oddeven()

##### In class session

## Creating sequences - using : 
1:10
1:10*2
seq(1,10,by = 0.25) # 
seq(100, 300, length.out = 10) # This is similar to linspace function in Matlab

## Repetition function
rep(5,100000000)

## Documentation
?mean
help(mean)
# NA reserved word in R to indicate missing values
example("mean") # runs the example code in the help documentation
example("boxplot")

## To install packages
install.packages("tseries")
library(tseries)
nifty <- get.hist.quote(instrument = "^NSEI", start = "2007-09-17", quote = "Close")
sensex <- get.hist.quote(instrument = "^BSESN", start = "2007-09-17", quote = "Close") # The instrument detial can be taken from Yahoo! finance
plot(nifty)
plot(sensex)

## Data Frame
tips<-read.csv(file = "tips.csv", stringsAsFactors = TRUE) # by default strings are read as factors, by changing the stringsasfactors argument you can modify the behaviour
str(tips)
# ~ goes to MyDocuments in windows and home directory in linux/mac
dim(tips)
summary(tips) # descriptive stats 
hist(tips$tip) # histogram 
table(tips$day, tips$sex) # two way table 
barplot(table(tips$day, tips$sex)) # Barplot for a table
boxplot((tips$tip))

## Subset of dataframe
#df[row filter , column filter]
tips[1:3, 1:3]
tips_sat<- tips[tips$day=='Sat',] # DON'T FORGET THE COMMA 
summary(tips_sat)
tips.sun<- subset(tips, day=='Sun')
summary(tips.sun)

##Creating a new variable
tips$tipsratio<-(tips$tip/tips$total_bill)*100
summary(tips)

## Bivariate analysis
aggregate(tips$tip, by=list(tips$day), FUN= function(x)
  c(avg=mean(x), sd=sd(x), max=max(x) ) )


aggregate(tips$tip, by=list(tips$day), FUN=mean)

aggregate(tips$tip, by=list(tips$day), FUN= function(x)
  c(avg=mean(x), sd=sd(x), max=max(x) ) )

boxplot(tips$tip~tips$day)
