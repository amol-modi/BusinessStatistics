#====================================#
# Statistics for Managers, Session 6 #
# 		(with solutions)			 #
#====================================#

#### Bi-variate relationships 
# ======================

### When one variable is a factor and the other numeric 

crime <- read.csv(file.choose(), header=T) # We already know that city is a factor
aggregate(vcrime ~ city, data = crime, FUN="mean", na.rm=T)
aggregate(vcrime ~ city, data = crime, FUN="sd", na.rm=T)

# Using the psych package, you can get all measures 
library(psych)
describeBy(crime$vcrime, crime$city)

# But to get better output, we can use the popular dplyr package
library(magrittr)
library(dplyr)
crim_sum<-crime %>% group_by(city) %>% summarize(avg = mean(vcrime), std = sd(vcrime), trimmed_avg = mean(vcrime, trim=0.1)) 

### When both variables are numeric 

## Correlation 
plot(crime$vcrime, crime$pcrime, main="Correlation between property crimes and violent")
abline(lm(crime$pcrime ~ crime$vcrime), lwd=3, col="steelblue") # Looks like a near perfect correlation
# Note the lm function. It is for linear models. We will focus on this in the second half of this course. For now, just use it to plot an abline. 

# However, we can also get a numeric value for the correlation 
cor(crime$vcrime, crime$pcrime) # .94  This is an unusually high correlation! 

# To run all correlations in a dataframe we need to select all relevant numeric variables (columns). This is called a "Correlation matrix"
cormat <- cor(crime[,2:5]) # Select only numeric variables. Otherwise, you'd get an error message. 
round(cormat, 2) # Rounded to 2 decimals 

# We can now get scatterplots of all numeric variables
pairs(crime[,2:5])
# Or use variable names -- easier to sort 
pairs(~year+population+vcrime+pcrime, data=crime)

# We can also use even more cool plotting techniques to illustrate the correlation 
library(corrplot)
corrplot(cormat, method="circle")
corrplot(cormat, method="circle", addCoef.col="grey") # With correlation values; # If you want to generate interactive correlation plots, check out the qtlcharts package!

# Finally, we can compare correlations by city. There are various ways to do this. 
# We'll start with a simple one.  
chi <- crime[crime$city=="Chicago",]
la <- crime[crime$city=="Los Angeles",]
ny <- crime[crime$city=="New York",]
cormat_chi <- round(cor(chi[,2:5]),2) 
cormat_la <- round(cor(la[,2:5]),2) 
cormat_ny <- round(cor(ny[,2:5]),2) 
cormat_chi ; cormat_la ; cormat_ny

# Alternatively, we could use the car package
library(car)
scatterplotMatrix(~year+population+vcrime+pcrime|city, data=crime, main="Correlations by city")

### Answers to quiz (Part I)
#--------------------

# 1. 
realestate <- read.csv(file.choose(), header=T) 

# 2. 
str(realestate)
dim(realestate)
View(realestate)
# All look okay, no additional preparation needed. 

# 3. 

quantile(realestate$price, probs = .75, na.rm=T) 
# 304631.2 
quantile(realestate$lotsize, probs = c(.1,.9), na.rm=T)
#  10%    90% 
# 2968.3 7953.0 

# 4. 
library(psych)
describe(realestate)
par(mfrow=c(3,2))
plot(density(realestate$lotsize))
plot(density(realestate$price)) 
plot(density(realestate$stories)) # Doesn't seem to be a continuous variable. It's more like a factor. Let's check a table
table(realestate$stories)

# Boxplot could provide additional support, if needed 
boxplot(realestate$lotsize) # Seems to be right-skewed, with some outliers. 
boxplot(realestate$price) # Seems to be somewhat right-skewed, with many outliers.
boxplot(realestate$stories) # Again, this does not look to be a numeric variable. the median line is at the 75th percentile. 

# Let's examine the two variables that may be normally distributed. 
qqnorm(realestate$lotsize)
qqline(realestate$lotsize)
qqnorm(realestate$price)
qqline(realestate$price)

# 5. 

# lotsize: 
# This is a numeric variable. The median (4400) is much smaller than the mean (5041.86), indicating outliers with much bigger lots. The trimmed mean further confirms that there are outliers. The min. value being much closer to the mean provides additional support to this finding. Finally, the skew statistic indicates a right skew (positive and > 0); and kurtosis is somewhat lower than normal distribution (<3). The density plot and qqnorm plots provides very clearly indicate a skew to the right, and that lotsize is not a normally distributed variable. 

# price: 
# This is a numeric variable. The median (231000) is smaller than the mean (255473.23), indicating pricey outliers. The trimmed mean further confirms that there are outliers. The min. value being somewhat closer to the mean provides additional support to this finding. Finally, the skew statistic indicates a right skew (positive and > 0); and kurtosis is somewhat lower than normal distribution (<3). The density plot and qqnorm plots provides very clearly indicate a skew to the right, and that this price is not a normally distributed variable. 

# stories: 
# Although this is a numeric variable, it is probably preferable to regard it as a factor. Looking at the table it shows that the majority of houses (about 89%) have one or two stories, while a few (about 7%) have 3, and even less (4%) have 4 stories. 


### Answers to quiz (Part II)
#--------------------

# 1. 
aggregate(price ~ airco, data = realestate, FUN="mean", na.rm=T)
323402.5 - 228263.5
# $95139 

# 2. 
library(psych)
describeBy(realestate$price, realestate$airco) # This gives all the descriptive statistics 
boxplot(realestate$price ~ realestate$airco, main = "Distribution comparison of houses based on air conditioning", col=c("red", "blue"))
plot(density(realestate$price[realestate$airco=="yes"]), col="blue", ylim=c(0, 6.316e-06), lwd=3) 
# There is no need for you to know how to compute the ylim for the exam. But I have found it by looking at the min/max values of the y in the second density function 
lines(density(realestate$price[realestate$airco=="no"]), col="red", lwd=3)

# The statistics and plots indicate a considerable differnece in the price of houses with and without air conditioning. 
# We already know from 1. that there is substantial differnece in mean prices. We now know that houses with air conditioning are more normally distributed and have less outliers. As we can see in the boxplot, the price difference is more spread in these houses (wider range). Conversely, houses without air conditioning, have more outliers, and this variable is skewed to the right, as indicated by the density plot, a higher skew statistic (1.49 compared to 1.01). At the same time, because the prices of houses with air conditioning is more spread out, the kurtosis of this variable is considerably lower than a normal distribution (<3), whereas houses without a/c are a little less spread out, resulting in a higher, right-skewed peak. 

# 3. 
cor(realestate$price,realestate$lotsize) 
# 0.54 (that's a high correlation)

# 4. 
renum <- realestate[,c(1:4, 10,12)]
cormat <- cor(renum) # Select only numeric variables. 
round(cormat, 2) # Rounded to 2 decimals 
# lotsize and price (cor = .54)  # These two variables are the most highly correlated 
# bedrooms and stories (cor = .42)
# bathrooms and price (cor = .50) # These two variables are the second most highly correlated 

# And for fun, let's visualize these correlations 
pairs(renum) # Makes little sense because of the properties of most numeric variables in the data 
library(corrplot)
corrplot(cormat, method="circle", addCoef.col="grey") 