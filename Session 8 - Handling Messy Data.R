#====================================#
# Statistics for Managers, Session 8 #
#====================================#

# Messy column names
#======================================
df <-data.frame(x.1=c("a",seq(1:4)),x.2=c("b",rnorm(4,mean=2,sd=.2))) 
head(df) 
names(df) 
colnames(df)<-c("a","b") # Assign new column names 
df <- df[-1,] # Drop the incorrect row(s) 

# Multiple columns for a single variable 
#======================================
df$Q1 <- rnorm(4, mean=0.3, sd=0.1)
df$Q2 <- rnorm(4, mean=0.3, sd=0.1)
df$Q3 <- rnorm(4, mean=0.3, sd=0.1)
df$Q4 <- rnorm(4, mean=0.3, sd=0.1)
df

# Let's fix this
library(tidyr)
gather(df, quarter, performance, -c(a,b)) # df = data frame; quarter, performance the two new columns, -c(a,b), include all existing columns except a and b. 
df # much better! 
# More on reshaping data at: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
# https://cran.r-project.org/web/packages/reshape2/reshape2.pdf

# Get rid of odd characters
#=====================================
c1<-c("111", "$123", "145.0") 
class(c1) 
as.numeric(c1) # Oops. Let's try again: 
oddchars <- "\\$|\\,|\\*" # Add any odd characters here with an | 
grep(oddchars, c1, ignore.case = T) # Matches pattern
c1<-gsub(oddchars, "", c1) # Aha!
c1<- as.numeric(c1) 

# Missing data 
#======================================
age <- c(20, 30, NA, -99) 
mean(age) # We already know that this isn't going to work
is.na(age) # Logical
which(is.na(age)) # Gives location of the missing data 
mean(age, na.rm=T) # Not that easy
which(is.na(age)|age<18) # So we need to use common sense, and we should also do this: 
age[age==-99]<-NA 
mean(age,na.rm=T)

#Time formatting
#======================================
d1<-c("2016/01/01", "2016/01/02", "2016/01/03", "2016/10/04")
class(d1) 
summary(d1)
d1<-as.Date(d1,"%Y/%m/%d")
class(d1) # New class: date. 

# Let's try it with another example: 
d2<-c("16-Jan-01", "16-Jan-02", "16-Jan-03", "16-Oct-04")
d2<-as.Date(d2,"%y-%b-%d")
class(d2)

# But what if we want to extract year/month/day from a date? 
# It's best to use a package. The fastest and best today is this one:  
library(lubridate)
d1<-c("2016/01/01", "2016/01/02", "2016/01/03", "2016/10/04")
d1<-ymd(d1) # That was easy! And these are easy (and very useful) too: 
y<-year(ymd(d1)) 
m<-month(ymd(d1)) 
d<-day(ymd(d1)) 
w<-week(ymd(d1)) 
q<-quarter(ymd(d1)) 
