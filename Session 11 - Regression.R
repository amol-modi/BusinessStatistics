#=======================================#
# Statistics for Management, Session 11 #
#=======================================#

# Load data
#============================
cars <- read.csv("usedcars.csv", header=T)
realestate <- read.csv("realestate.csv", header=T)


# Data cleaning 
#============================
cars$price<-as.numeric(gsub(",", "", cars$price))
cars$mileage<-as.numeric(gsub(",", "", cars$mileage))
cars$year<-gsub(",", "", cars$year)
summary(cars$year)
cars$age<-abs(as.numeric(cars$year)-2012) # Makes interpretation easier  

## Simple linear regression 
#============================
# Our DV is price

mod1<-lm(price ~ mileage, data=cars)
mod1
summary(mod1) 
confint(mod1)
round(confint(mod1),3)

# To predict specific values based on a model
round(coefficients(mod1),3) 
# For example, the predicted price of a car with 100k miles: 17091.52+(-0.093)*100000 = 7791.52
predict(mod1, data.frame(mileage =(c(5000 ,10000 ,100000) )), interval ="confidence", level=0.95) # the difference is due to rounding in the line above

# How well did our model perform? 
head(cars$price)-head(predict(mod1)) # or using this function 
(head(cars$price)/head(predict(mod1)))*100-100
summary(residuals(mod1))
plot(cars$price~cars$mileage)
abline(mod1, col="red", lwd=3)

# R-sqrd
summary(mod1)$adj.r.squared # The simple (single IV) model explains about 65% of the variance 

# Diagnostics 
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 
plot(mod1)

# or 
dev.off()
plot( predict (mod1), residuals (mod1)) # we are looking for a "no pattern"/non-linearity 
# finding outliers can be done this way as well
plot(hatvalues(mod1)) 
identify(hatvalues(mod1), col="red")
# or because it looks like we have 2 outliers, we can do this 
tail(sort(hatvalues(mod1)), n=2)

outliers <- c(90, 149)
cars1<-cars[-outliers,]
mod2<-lm(price ~ mileage, data=cars1) 
summary(mod2); summary(mod1) # Yes, we can see some improvement 

# Exercise  
#============================
re_mod1 <- lm(price ~ lotsize, data=realestate)

# The predicted price for lot 500, 1000, 2000, at 99% confidence 

predict(re_mod1 , data.frame(lotsize=(c(500 ,1000 ,2000) )), interval ="confidence", level=0.99) 

#       fit      lwr      upr
#1 140004.4 116968.8 163040.0
#2 152716.0 131717.4 173714.6
#3 178139.3 161025.0 195253.6

# Adjusted R-sqrd is: 
summary(re_mod1)$adj.r.squared 
# [1] 0.291975   # This is not a high R-sqrd value. We will need to include other IVs to build a better model. 

# Outliers 
tail(sort(hatvalues(re_mod1)), n=5)

# regress without 5 most "extreme" outliers 
realestate1 <- realestate[-c(491,366,383,365,369)]
re_mod2 <- lm(price ~ lotsize, data=realestate1) # Adjusted R-sqrd hasn't really changed because the data are heavily skewed 
summary(re_mod2)$adj.r.squared 

# regress with logged DV and IV 
re_mod3 <- lm(log(price) ~ log(lotsize), data=realestate) # Adjusted R-sqrd hasn't really changed because the data are heavily skewed 
summary(re_mod3)$adj.r.squared 
# [1] 0.3257167  # Yes, this is an improvement

#============================


## Multiple linear regression 
#============================
# It is the same syntax as simple linear regression. Only add (with a plus sign) other IVs. You may also simply write a dot (.) to include all variables. Let's start with one more numeric IV. 

mod3 <- lm(price ~ mileage+age, data=cars)
summary(mod3)

# we can specify each of the variables
# and add factors (dummy variables)
mod4 <- lm(price ~ mileage+age+transmission+color, data=cars) # color does not explain much. We'd better drop it 

# And let's also organize the data and include all relevant variables 
cars2 <- cars[,c(2:4,6,8)] # drops year (same as age), and date 
mod5 <- lm(price ~. , data=cars2)
summary(mod5) # Looks like tranmission also does little, so let's re-run without it 
mod5 <- lm(price ~ age+mileage+model , data=cars2)
summary(mod5)
summary(mod5)$adj.r.squared
summary(mod3)$adj.r.squared
pct_improved <- round(((summary(mod5)$adj.r.squared/summary(mod3)$adj.r.squared)-1)*100,2)
pct_improved 

# But we haven't really checked if regression assumptions are met 
par(mfrow=c(2,2))
plot(mod5)
# Looks like we need to remove a few outliers
cars3 <- cars2[-c(90, 1, 2, 149),]
mod6 <- lm(price ~ age+mileage+model , data=cars3)
summary(mod6)

# And what about the DV/IV being normally distributed? 
par(mfrow=c(2,2))
plot(density(cars3$price))
plot(density(cars3$mileage))
plot(density(cars3$age))

dev.off()
# Let's explore a few possible transformations 
par(mfrow=c(2,2))
plot(density(sqrt(cars3$mileage))) 
plot(density(sqrt(cars3$age))) 
plot(density(log(cars3$mileage))) 
plot(density(log(cars3$age))) 
# Square root seems better in this case, so, one last time! 
mod7 <- lm(price ~ sqrt(age)+sqrt(mileage)+model , data=cars3)
summary(mod7)
# Okay, maybe we still need to fine tune a bit by removing a bit more outliers... 

# One more step, though: multicollinearity. 
# Let's examine the relationship between age and mileage

cor(cars3$age, cars3$mileage) #[1] 0.7519428 highly correlated. 

# But a more precise analysis is using VIF (Variance Inflation Factor)
library(car) 
vif(mod7) 
sqrt(vif(mod7)) > 2 
# Conservatively, we would be concerned of sqrt(vif) > 2 