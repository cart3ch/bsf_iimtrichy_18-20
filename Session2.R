##### SESSION 2 BSF Gr.2 - June 11 ######

#Add comments by appending <#> to the line
# <Ctrl + 1> Move cursor to script window
# <Ctrl + 2> Move cursor to console window

##### Working directories
# getwd()   -> shows the current working directory
# setwd("") -> to change the working directory
# Note of the back slash cahracter '\'. R by default uses unix slash for file structure. In order to write window folder use '\\'

#Assignment of variables
x <- 2 # assignment of variables, use this for outside assignments  
y = 'ABC' # assignment of variables, use this for parameters
z <- TRUE # Boolean assignment
str(x) # gives the structure of the variable, 
str(y) # str() not interesting for atomic datatypes but very useful for dataframes, tibbles etc.
str(z)

# Reserved words
# TRUE, FALSE, NULL
1/0 #Inf
(1/0)/(1/0) #NaN

# Vectors
v1 <- c(5,6,9)
v1
v1[2]

v2<-c('B','S','F')
v2
v2[c(1,3)] # Shows subset of the vector
v2[c(3,1,1,3)]

# Coercion
c(2,4,TRUE) # Coercion Hierarchy: Char>>Numeric>>Boolean
c(2,4,'5')
c("a","b",TRUE)

# List
l1<- list("a", 1, TRUE)
l1
l2 <- list("name"="a", "exp"=1, "status"=TRUE) #naming 
l2
l2$exp #calling
l2[[2]]

# DataFrame

df<- data.frame(a=c(2,5,9), b= c("Alpha",'b',"c"))
df
str(df)

tips<- read.csv("tips.csv")
str(tips)
View(tips)
summary(tips)
