#====================================#
# Statistics for Managers, Session 4 #
#====================================#
cars <- read.csv(file.choose(), header=T) 
cars$mileage<-as.numeric(sub(",", "", cars$mileage))
cars$price<-as.numeric(sub(",", "", cars$price))

realestate <- read.csv(file.choose(), header=T)  # I've already loaded the car data. 

### Plotting univariate numeric variables 
# ======================

# Boxplot 
boxplot(cars$mileage, main="Used Car Mileage")
boxplot(cars$price, main="Used Car Prices", col="orange")

# Plotting distributions 
carsnum<-cars[,c("price", "mileage")]
detach(cars); attach(carsnum)
hist(price)
hist(price, probability=TRUE)
hist(price, breaks=20)
hist(price, breaks=10, col=c("steelblue", "red"))
box()

# Now for mileage
hist(mileage, col=c("steelblue", "red"))
box() 

# Now add rug plot (actual data elements)
rug(jitter(mileage), col="darkgray")

# Another solution to the binning problem is to use a "smoother": the kernel density plot
den<-density(mileage)
plot(den)

# Now combine all three plots together: 
hist(mileage, col=c("steelblue", "red"), freq=F)
rug(jitter(mileage), col="darkgray")
lines(density(mileage), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box() 

# From the numerical parameters (measures), and plots
# we have a reasonable sense of variable distribution. Let's take one more step: 
# Using Quantile-Quantile plot  
qqnorm(mileage)
qqline(mileage, col="red", lwd=2)

qqnorm(price) 
qqline(price, col="red", lwd=2) # a lot better

# Multiple plots in the same output 
dev.off()
par(mfrow=c(2,2)) 
hist(mileage, col=c("steelblue", "red"), freq=F)
rug(jitter(mileage), col="darkgray")
lines(density(mileage), col="yellow", lwd=3)
box() 
hist(price, col=c("steelblue", "red"), freq=F)
rug(jitter(price), col="darkgray")
lines(density(price), col="yellow", lwd=3)
box() 
boxplot(mileage, main="Used Car Mileage", col="green")
boxplot(price, main="Used Car Prices", col="orange")

# Save output into a pdf file
pdf("plots.pdf",width=7,height=5)
par(mfrow=c(2,2)) 
hist(mileage, col=c("steelblue", "red"), freq=F)
rug(jitter(mileage), col="darkgray")
lines(density(mileage), col="yellow", lwd=3)
box() 
hist(price, col=c("steelblue", "red"), freq=F)
rug(jitter(price), col="darkgray")
lines(density(price), col="yellow", lwd=3)
box() 
boxplot(mileage, main="Used Car Mileage", col="green")
boxplot(price, main="Used Car Prices", col="orange")
dev.off()


## One last example
par(mfrow=c(1,2))
grades<-grades[complete.cases(grades),]
hist(grades$HW1, col=c("steelblue", "red"), freq=F)
rug(jitter(grades$HW1), col="darkgray")
lines(density(grades$HW1), col="yellow", lwd=3)
box()

hist(grades$HW2, col=c("steelblue", "red"), freq=F)
rug(jitter(grades$HW2), col="darkgray")
lines(density(grades$HW2), col="yellow", lwd=3)
box()


#### Answers to quiz
#--------------------

# Replicate the code above using relevant variables. 
 
### Examine two variables   
# ======================

# Two categorical variables 
#-----------------------
# Numerically, we use a two-way table 
tab <- table(emp$Training, emp$Performance_change)
tab

# Graphically, we have various options
plot(tab) # Same as:
mosaicplot(tab, main="Performance Change After Training", col=c("lightgreen", "orange", "red"))

# But barplot provides more information, such as frequency 
barplot(tab, col=c("coral", "greenyellow"),main="Performance Change After Training") # For more color names see: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 

# Now that we can see numeric information, we should also check the proportions of the table (percentages)
tab1<-prop.table(tab, margin=2) # Uses column margin (proportion); margin=1 uses row margins (proportion). 
barplot(tab1, col=c("coral", "greenyellow"), main="Performance Change After Training") 
	# Hmmm.... It unclear which is price and which is with and without training. We need to add a legend!
legend("bottomright", 
    legend = c("No Training", "Training"), 
    fill = c("coral", "greenyellow"))	  # In this particular plot the bottom right area is probably best for adding a legend. 
	
# Answers to Quiz
#-----------------------
tabr<-table(realestate$prefarea, realestate$recroom)
barplot(prop.table(tabr,2), xlab="Preferred Area", col=c("darkgrey", "darkgoldenrod1"), main="Recrooms by Location")
legend("bottomright", 
    legend = c("No Recroom", "Recroom"), 
    fill = c("darkgrey", "darkgoldenrod1"))	  
	
# Two numeric variables
#-----------------------
# Simple! Let's go back to the used cars data
plot(cars$mileage,cars$price,  col="blue")
plot(cars$price~cars$mileage, col="red") # Note the differences! It may be better to use this format. It is the same format as modelling. 
identify(cars$price~cars$mileage, labels=rownames(cars))
# Now let's make this plot a little more understandable
plot(cars$price~cars$mileage, col="red", 
	main="Relationship of mileage to the price of used cars", 
	xlab="Mileage", 
	ylab="Price", 
	pch=16) 
abline(lm(cars$price~cars$mileage), col="darkgreen", lwd=2.5) # Regression line slope is distinct, and not too far from observations 
# We can also add a "smoother"
lines(lowess(cars$price~cars$mileage), col="steelblue", lwd=2.5) # Smoother is similar to regression line, indicating a linear relationship
lines(lowess(cars$price~cars$mileage, f=0.1), col="orange", lwd=2.5) # Overfitting

# Answers to Quiz
#-----------------------
plot(realestate$price~realestate$lotsize, col="red", 
	main="Relationship of lotsize to the price of houses", 
	xlab="Lotsize", 
	ylab="Price", 
	pch=16) 
abline(lm(realestate$price~realestate$lotsize), col="darkgreen", lwd=2.5) 
lines(lowess(realestate$price~realestate$lotsize), col="steelblue", lwd=2.5) 
identify(realestate$price~realestate$lotsize) # 93 378 419
realestate$prefarea[c(363,378, 419)] # no yes yes -- the "lower" price is not in a pref area, the other two are. 

# Categorical and numeric variable  
#-----------------------
# Load the employee training data set 
emp <- read.csv(file.choose(), header=T)

# Numeric stats
table(emp$Performance_num, emp$Gender) # Not great. We will explore aggregate stats of a numeric variable as they relate to a categorical variable. 

# Boxplots are a good example of this procedure -- they help show differences in categories as they relate to a numeric variable.  
boxplot(Performance_num ~ Gender, data=emp, main="Performance Change After Training by Gender", 
	xlab="Gender", ylab="Performance",
	col=c("orange", "lightblue4")) # Better! 
	
# Density plots, histograms etc. can also be used for the same purpose! 
m<-emp[emp$Gender=="Male",]
f<-emp[emp$Gender=="Female",]
plot(density(m$Age), col="red", lwd=2.5, main="Distribution of Age by Gender")
lines(density(f$Age), col="blue", lwd=2.5)

plot(density(m$Performance_num), col="red", lwd=2.5, main="Distribution of Performance Change by Gender")
lines(density(f$Performance_num), col="blue", lwd=2.5) # There is a more noticable difference. 

#### Answers to quiz
#--------------------
boxplot(realestate$price~realestate$fullbase, main="Distribution of House Price by Having a Finished Basement", xlab="Finished Basement", col=c("orange", "lightblue4"))
identify(realestate$price~realestate$fullbase)
realestate[c(332,337,378,419,93,338),] # Among the obesrvations that could be made: Obviously, priced much higher than the median price. There are higher proportion in the prefered area. There are also more outliers in those that do not have a finished basement... 

