#======================================#
# Statistics for Management, Session 9 #
# 		(with solutions)			   #
#======================================#

### Sampling and confidence intervals 
#==========================
library(psych)
describe(donors$donation_total)
tot<-donors$donation_total
hist(tot) # Can't see much here
hist(tot, breaks=10) # Still unclear 
hist(tot, breaks=100) # Vast majority of very small donations and a few relatively large donations. 
hist(tot, breaks=100, xlim=c(0,1000)) # Even better. Still, this is an extremely right-skewed variable. 

### Sampling 
#==========================
# But we rarely have the entire population! Instead, let's try to "estimate" donations using a sample of n=30  
set.seed(123)
n <- 30
tot_samp <- sample(tot, n)
mean(tot_samp)
mean(tot)-mean(tot_samp) #-18.07; Not good, but because of the skewness, we really care about the median 
median(tot)-median(tot_samp) # 5; Not bad! 
# As a reminder
length(tot)
100*length(tot_samp)/length(tot)

# Answers 
#---------
price<-realestate$price
plot(density(price)) # It is right-skewed
#par(mfrow=c(2,2))
set.seed(123) # If you haven't done so earlier in the session 
n <- 30
price_samp <- sample(price, n)
plot(density(price_samp)) # It is right-skewed, quite similar to the distribution of price
# And to compare the two distributions 
plot(density(price/1000), lwd=3, col="gray", xlab="")
lines(density(price_samp/1000), lwd=3, col="blue")
legend(650,0.0049, c("N=500", "n=30"), col=c("gray", "blue"), lwd=c(3,3), cex=0.8)
box()
# Now for the difference in estimated the parameters 
mean(price)-mean(price_samp) #-28226.85
median(price)-median(price_samp) #-15400
sd(price)-sd(price_samp) #-18292.58

# Estimation here is not great. We need to do more work. 

# Increasing the sample size
#==========================
# Let's start by increasing the sample size 
n <- 150
tot_samp <- sample(tot, n)
mean(tot)-mean(tot_samp) # A bit better: 12.03 as opposed to -18.07 with n=30. But again, we care more about the median. 
median(tot)-median(tot_samp) # 0! 
sd(tot)-sd(tot_samp) # 56.90 

# Answers 
#---------
n <- 100
price_samp <- sample(price, n)
mean(price)-mean(price_samp) #-11650.82 compared to -28226.85, so over twice more accurate. 
median(price)-median(price_samp) #-4812.5 compared to -15400, about three times more accurate. 
sd(price)-sd(price_samp) #-3284.537 compared to -18292.58, over five times more accurate. 

# More samples 
#==========================
# Initialize a vector for each parameter 
n <- 30 
sample_means <- rep(NA, 100)
sample_medians <- rep(NA, 100)
sample_sds <- rep(NA, 100)

# Run the for loop
for (i in 1:100) {
  s<-sample(tot, n)
  sample_means[i]<-mean(s)
  sample_medians[i]<-median(s)
  sample_sds[i]<-sd(s)
  }
 
mean(tot)-mean(sample_means) #-4.7; Almost perfect.  
median(tot)-mean(sample_medians) #-1.28; Not as good as the larger sample size. 
sd(tot)-mean(sample_sds) # 29.15; A lot closer. 
 
# One more time with more samples  
sample_means <- rep(NA, 10000)
sample_medians <- rep(NA, 10000)
sample_sds <- rep(NA, 10000)

for (i in 1:10000) {
  s<-sample(tot, n)
  sample_means[i]<-mean(s)
  sample_medians[i]<-median(s)
  sample_sds[i]<-sd(s)
  }

mean(tot)-mean(sample_means) #0.4 ; Awesome! 
median(tot)-mean(sample_medians) #-0.53; Better 
sd(tot)-mean(sample_sds) # 45.14; Not as good. More samples don't seem to matter. 

# Draw histograms of sample parameters 
par(mfrow=c(1,3))
hist(sample_means); hist(sample_medians); hist(sample_sds) # Not really a normal distribution. This is because we have extreme values. Increasing n will take care of this 
dev.off()
n <- 150
sample_means <- rep(NA, 10000)

for (i in 1:10000) {
  s<-sample(tot, n)
  sample_means[i]<-mean(s)
  }

hist(sample_means) # Much closer to a normal distribution, even for an extremely skewed distribution as donation total.  

# Answers
#-------------------------
n <- 100 
sample_means <- rep(NA, 10000)
sample_medians <- rep(NA, 10000)
sample_sds <- rep(NA, 10000)

# Run the for loop

for (i in 1:10000) {
  s<-sample(price, n)
  sample_means[i]<-mean(s)
  sample_medians[i]<-median(s)
  sample_sds[i]<-sd(s)
  }

mean(price)-mean(sample_means) #-119.34 ; Awesome! 
median(price)-mean(sample_medians) #-1393.4; Better 
sd(price)-mean(sample_sds) # 280; Not bad. 

par(mfrow=c(1,3))
hist(sample_means); hist(sample_medians); hist(sample_sds) # Yes, the seem to be normally distributed. 

# Confidence Interval 
#==========================
# What is the a typical donation?  
tot<-donors$donation_total

# We start by taking a sample of 50, and computing the SE 
n <- 200 
samp <- sample(tot, n)
mean(samp)-mean(tot) #-7.84 # not a big difference, but donation amounts are small 
median(samp)-median(tot) # 0 ; Wow! But this is a large sample size (10%), and, not considering a few outliers, there isn't much variability. Let's reduce it to a more "manageable" size: 
n <- 200
samp <- sample(tot, n)
# Now compute 95% CI 
se <- sd(samp)/sqrt(n)  
lbound <- mean(samp) - 1.96*se
ubound <- mean(samp) + 1.96*se
c(lbound, ubound) #51.16897 84.68513; Pretty good. 

# Now let's try it iteratively  
sample_means <- rep(NA, 200) # We only have 500 observations, and we cannot exceed 10% 
sample_sds <- rep(NA, 200)
n <- 200

# For loop goes here:
for (i in 1:n) {
s <- sample(tot,n)
sample_means[i]<-mean(s)
sample_sds[i]<-sd(s)
}

lbound <- sample_means - 1.96 * (sample_sds/sqrt(n))
ubound <- sample_means + 1.96 * (sample_sds/sqrt(n))


# How about we plot it? 
plot(c(lbound, ubound)) # Not very informative. 
# Let's try the custom plot_ci function 
source("plot_ci.R") # This is how we load/run the content of a script in R 
tot_mean <- mean(tot)
plot_ci(lbound, ubound, tot_mean) # Good! 

#### Additional options for highly skewed data (none of which are perfect)

# Option 1: Drop outliers 
dev.off()
plot(density(tot[tot<100])) # Not quite normal, but let's see if this works. 
length(tot[tot<100]) #1617
tot_trim<- tot[tot<100]
samp <- sample(tot_trim, n)
# Now compute 95% CI 
se <- sd(samp)/sqrt(n)  
lbound <- mean(samp) - 1.96*se
ubound <- mean(samp) + 1.96*se
c(lbound, ubound) # 18.18576 25.44244; Still not great. 

# Option 2: Use non-parametric bootstrap method 
library(simpleboot)
library(boot)
bt_mean<-one.boot(tot, mean, R=2000, trim=0.2) # This is  s l o w  (using student bootstrap will take longer)
boot.ci(bt_mean, type=c("perc", "bca")) # 95%   (27.46, 32.13 )   (27.53, 32.18 ) ; Not very good. 

# How about trying CI for a median? 
bt_median<-one.boot(tot, FUN="median", R=1000) 
boot.ci(bt_median, type=c("perc", "bca")) # Note the error message! This is the best we can do with such a variable without heavy "trimming", or transformations -- we will talk about this later in the course. 

# Answers
#-------------------------
# NA - Tabulated and plotted answers


# Hypothesis testing
#==========================
x <- seq(from=0, to=30, by=0.1)
demo_null<-t.test(x, alternative="two.sided", mu=14.5, conf.int=0.95)
demo_null
# We fail to reject the null hypothesis 

# Let's load the cars data 
cars <- read.csv("cars.csv", header=T)
cars$year<-as.numeric(gsub(",", "", cars$year))

# Cars models earlier than 2010 will have the same mean price as cars 2010 and after

cars$agecat[cars$year<2010]<-"oldies" 
cars$agecat[cars$year>=2010]<-"newies" 
mean(cars$price[cars$agecat=="oldies"])

# So, the null hypothesis is that mu=11196.07 for newies
price_null <- t.test(cars$price[cars$agecat=="newies"], alternative="greater", mu=11196.07, conf.int=0.95)
price_null
# We reject the null. 
# Actually, the better (and easier) way to do it is to compare samples from both populations: 
price_hypothesis_2samp <- t.test(cars$price[cars$agecat=="newies"], cars$price[cars$agecat=="oldies"])
price_hypothesis_2samp

# Note that we can access every bit of result directly: 
names(price_hypothesis_2samp) # shows various result and function parameter options, e.g., 
price_hypothesis_2samp$p.value 
# Or even better 
price_hypothesis_2samp[["p.value"]] 

# Let's plot the difference 
hist(cars$price[cars$agecat=="oldies"], main="Testing price differences")
hist(cars$price[cars$agecat=="newies"], col="steelblue", add=TRUE)
legend("topleft", c("oldies", "newies"), col=c("white", "steelblue"), pch=c(19,19), cex=0.8)

# Answers
#-------------------------
t.test(realestate$price[realestate$stories==2], realestate$price[realestate$stories>2], conf.int=0.99) # We reject the null 
t.test(realestate$price[realestate$stories==2], realestate$price[realestate$stories<2], conf.int=0.95) # Again, we reject the null 