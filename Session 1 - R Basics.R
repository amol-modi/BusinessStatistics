#====================================#
# Statistics for Managers, Session 1 #
#====================================#

#1. Basics (orientation)
getwd()
# [1] "C:/Users/zack/Documents"
setwd("RData")
getwd()
# [1] "C:/Users/zack/Documents/RData"
installed.packages()
install.packages('MASS')


## Solution
# you can create a folder by using your computer's operating system, or through R: 
getwd()  
# [1] "C:/Users/zack/Documents"
dir.create("C:/Users/zack/Documents/IDS570") 
setwd("IDS570")
install.packages('ggplot2')



#2. Basics (operations)
5+6
# [1] 11
8*7
# [1] 35

10*5 / 0.5
# [1] 100 

71/14 
# [1] 5.071429

24 %% 5
#[1] 4

#3. Basics (assignment)
a <- 5
b <- 7
a*b 
# [1] 35

a <- "welcome"
b<-570
v<-c(a,b)
v
#[1] "welcome" "IDS 570" 
a<- 10
v<-c(a,b)
v
#[1]  10 570


## Solution
score <- c(4,3.7,4,3.5)
tests <- 4
avg.score <- score/tests
avg.score
score <- 4
avg.score
# It's content did not change because the object avg.score was unchanged 

#3. Basics (data types)
## numeric
a<-5
## character
b<-"hello"
## logical (TRUE FALSE or T F)
c<-FALSE 

## check data type 
class(a)
#[1] "numeric"
class(b)
#[1] "character"
class(c)
#[1] "logical"


## Solution
class(avg.score)
# [1] "numeric"
avg.score <- c("employee", avg.score)
avg.score
class(avg.score)
# [1] "character"
# The data type has changed  because we added a character value to avg.score 

#4. Basics (functions)
1:10
seq(1:10)
?seq()
seq(from=1, to=10, by=0.01)
x <- seq(from=1, to=10, by=0.01)
y <- round(x,1) # or
y <- round(seq(from=1, to=10, by=0.01),1)
head(y)
tail(y)


## Solution 
my.seq <- seq(from=7, to=100, by=4.05)
new.seq <- round(tail(my.seq,n=4),1)






## R Demos
demo(graphics)
install.packages("quantmod")
demo("chartSeries", package="quantmod")