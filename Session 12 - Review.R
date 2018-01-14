#=======================================#
# Statistics for Management, Session 12 #
#=======================================#

# Load data
#============================
cars <- read.csv("usedcars.csv", header=T)
realestate <- read.csv("realestate.csv", header=T)

# Data prep 
#============================
cars$price<-as.numeric(gsub(",", "", cars$price))
cars$mileage<-as.numeric(gsub(",", "", cars$mileage))
cars$year<-gsub(",", "", cars$year)
summary(cars$year)
cars$age<-abs(as.numeric(cars$year)-2012) # Makes interpretation easier  

# Exercise  
#============================
names(realestate)
# We have 11 IVs and 1 DV. Let's assume that we have already finished the EDA stage 
re_mod4 <- lm(price ~ ., data=realestate)
summary(re_mod4)  #We are getting a much higher adjusted R-sqrd than the single linear regression. Also, the highest effect is lotsize and bathrooms, but 
# surprisingly bedrooms are not quite significant (p>0.05)
# Could there be multicollinearity? 
sqrt(vif(re_mod4)) >2 # Looks okay. But, we can drop this variable, as well as recrooms. 

re_mod5 <- lm(price ~ lotsize+bathrms+stories+driveway+fullbase+gashw+airco+garagepl+prefarea, data=realestate)
summary(re_mod5)

dev.off()
par(mfrow=c(2,2))
plot(re_mod5) # we have a few outliers: 363,93,378, 103, 104

realestate1 <- realestate[-c(363,93,378, 103, 104),]
re_mod6 <- lm(price ~ lotsize+bathrms+stories+driveway+fullbase+gashw+airco+garagepl+prefarea, data=realestate1)
summary(re_mod6)
plot(re_mod6)

# one more time 
realestate2 <- realestate1[-c(330,162,419, 330, 162,419, 466,332),]
re_mod7 <- lm(price ~ lotsize+bathrms+stories+driveway+fullbase+gashw+airco+garagepl+prefarea, data=realestate2)
summary(re_mod7) # Brings it to adj-R-squared of about 0.7

# And, again we need to check for multicollinearity
library(car)
sqrt(vif(re_mod7))>2 # Still fine. We can stop here or continue finetuning. 

# ===============
# Transformations 
# ===============

# Building a scale
# ===============
dev.off() 
par(mfrow=c(2,2))
plot(density(realestate$bedrooms))
plot(density(realestate$bathrms))
plot(density(realestate$garagepl)) 
cor(realestate[,c(2,3,10)]) # It is okay for correlation to be somewhat lower, as long as there is a conceptual association 
realestate$rooms <- realestate$bedrooms+realestate$bathrms+realestate$garagepl # Create a scale of "rooms" 
realestate$rooms <- realestate$rooms-1 # Because 1 doesn't makes sense here 
plot(density(realestate$rooms)) # Looks more like a numeric variable! 

# Let's regress 
re_mod8 <- lm(price~rooms+driveway+stories+fullbase+gashw+prefarea, data=realestate) 
summary(re_mod8)
# Note that r-squared has dropped from the "optimal" model above. 
# But, we can be more confidence with this model because of the rooms IV is more robust, and we can (and should) fine tune this model. 

# Interpretation
round(re_mod8$coefficients,2)
# Holding all else constant, an addition of a "room" (using the new rooms scale), increases the price of a house, on average, by $30,303.45. 


## Numeric transformation 
# ===============

plot(density(realestate$price))
# We can try to identify the outliers. There seems to be a few here. However, we can also transform the variable. 
# The two most common transformations are log and sqrt. For $ variables, we typically use a log. But let's see the sqrt form. 
plot(density(sqrt(realestate$price))) # Yes, not much has changed
plot(density(log(realestate$price))) # log price looks good 

# What about lotsize? 
plot(density(realestate2$lotsize))
plot(density(sqrt(realestate2$lotsize)))
plot(density(log(realestate2$lotsize))) # Better

# Finally, can we also transform other odd variables like bathrooms? 
plot(density(realestate2$bathrms))
plot(density(log(realestate2$bathrms)))
plot(density(sqrt(realestate2$bathrms))) # Nope. This is not going to work. We can use this variable to build a scale, or, as you'll see below, convert it into a factor.  


# Regress
re_mod9 <- lm(price~log(lotsize)+rooms+stories+fullbase+prefarea, data=realestate)
summary(re_mod9)

# Interpretation
round(re_mod9$coefficients,2)
# Interpreting a transformed variable takes a little practice, but it is not too hard. When the DV is logged, for example, we take the exponent /
# exp() of the betas. When the IV is logged, we may interpret the effect on a logged scale. For a good review of how to interpret results from numeric transformations: 
# http://www.ats.ucla.edu/stat/mult_pkg/faq/general/log_transformed_regression.htm

# Convert numeric to a factor 
# ===============

# How many categories (levels)?
table(cars$age)
table(cars$age>3)
table(cars$age>2) # Seems best if we go with a binary factor 

# There are various ways to carry out this transformation. Here is one: 
cars$agecat[cars$age>2] <- "old"
cars$agecat[cars$age<=2] <- "new"
table(cars$agecat)

# Re-run the model with the transformed variable
mod5 <- lm(price ~ agecat+mileage+model , data=cars)
summary(mod5)

# Interpretation 
round(mod5$coefficients,2) # holding all other variables at their constant, the average difference between old and new cars is about $1,605. 

# How about more than just two categories? 
table(cars$mileage) # Not very useful 
summary(cars$mileage) 
# as a reminder, this is how we get quantiles
quantile(cars$mileage, c(seq(0,1, by=0.1)))
# but for now, we will use the 1st, 2rd, and 3rd quartiles, we we'll end up with 4 categories based on quantiles (0-1, 1-2, 2-3, 3-4)
cars$milcat<-cut(cars$mileage, c(0,27200.25,36385,55124.5,151479)) 
table(cars$milcat) # hard to understand
cars$milcat<-cut(cars$mileage, c(0,27200.25,36385,55124.5,151479), labels=c("very low", "low", "medium", "high")) 
table(cars$milcat) 

# Regress with the new factor 
mod6<- lm(price ~ agecat+milcat+model , data=cars)
summary(mod6) 

