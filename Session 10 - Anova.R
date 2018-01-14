#=======================================#
# Statistics for Management, Session 10 #
#=======================================#
setwd("C:\\Users\\Desktop\\Statistics for Management\\Session 10")

# Hypothesis testing skewed data 
#==================================
sales_senior<-read.csv("sales_seniority.csv", header=T)
plot(density(sales_senior$sales[sales_senior$Age=="Over 5 years"]), ylim=c(0,0.09), lwd=3)
lines(density(sales_senior$sales[sales_senior$Age=="5 years or less"]), col="blue", lwd=3)
legend("topright", c("Over 5 years", "5 years or less"), lwd=3, lty=1, col=c(1, "blue"))
median(sales_senior$sales[sales_senior$Age=="Over 5 years"]); median(sales_senior$sales[sales_senior$Age=="5 years or less"])

# Run a Wilcoxon-Mann–Whitney U-test
wilcox.test(sales_senior$sales[sales_senior$Age=="Over 5 years"],sales_senior$sales[sales_senior$Age=="5 years or less"], alternative="greater") # Don't worry about the error messages; Use paired=T for paired samples. 
 
# Chi-squared test for contingency tables 
#==================================
# Car dealership example 

# At 95% confidence, are customers of a car dealerships evenly distributed throughout the week

observed<-c(108,98,102,93,111,116,87)
expected <- rep(sum(observed)/7, 7)
chi_sq <- sum((observed-expected)^2/expected)
chi_sq
# Why not use a built-in function? 
dealership <- as.table(c(108,98,102,93,111,116,87))
dimnames(dealership)<-list(weekdays = c("M", "T", "W", "Th", "F", "S", "Su"))
chisq_test<-chisq.test(dealership)
chisq_test
chisq_test$p.value

# And what if the observation changes, for example, on Tuesday and Wednesday? 
dealership <- as.table(c(108,81,132,93,111,116,87))
dimnames(dealership)<-list(weekdays = c("M", "T", "W", "Th", "F", "S", "Su"))
chisq_test<-chisq.test(dealership)
chisq_test

## Exersice 
##=========
analysts <- as.table(c(98.4,72,96,81,77.5, 62,69.5,74.5))
dimnames(analysts)<-list(IDs = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"))
chisq_test<-chisq.test(analysts)
chisq_test
chisq_test$p.value # 0.047 ; This is above 0.01 so we cannot reject the null; We will reject the null at 0.05 level. 

analysts <- as.table(c(98.4,72,96,81,77.5, 69.5,74.5))
dimnames(analysts)<-list(IDs = c("A1", "A2", "A3", "A4", "A5", "A7", "A8"))
chisq_test<-chisq.test(analysts)
chisq_test #  0.1341393 Yes it will change. We cannot reject the null. 
##=========

# Animal shelter data 
dogshelter<-read.csv("dogshelter.csv", header=T)
table(dogshelter$AnimalType, dogshelter$OutcomeType)

# Is there a difference bewteen the outcome of dogs and cats
dogstab<- table(dogshelter$AnimalType, dogshelter$OutcomeType)
addmargins(dogstab,c(1,2))
pdogtab<-prop.table(dogstab)
round(pdogtab,2)
addmargins(round(pdogtab,2),c(1,2))
barplot(pdogtab, col=c("orange", "gray"))
legend("top", c("cats", "dogs"), lty=1,lwd=4, col=c("orange", "gray"))

# Now for the chi-square test 
chisq.test(dogstab)

# What if we reverse the rows and columns? 
dogstab1<- table(dogshelter$OutcomeType, dogshelter$AnimalType )
identical(chisq.test(dogstab1)$p.value, chisq.test(dogstab)$p.value)

# Let's examine another factor: pet's color 
table(dogshelter$Color)
table(dogshelter$Color=="Black/White") # a little over 10%
dogshelter$color_cat[dogshelter$Color=="Black/White"]<-"BW"
dogshelter$color_cat[dogshelter$Color!="Black/White"]<-"NotBW"
table(dogshelter$color_cat) # Looks good
col_tab<-table(dogshelter$color_cat, dogshelter$OutcomeType)
chisq.test(col_tab)

# Maybe we need to add another common color? Tricolor
dogshelter$color_cat[dogshelter$Color=="Black/White"|dogshelter$Color=="Tricolor"]<-"BWT"
dogshelter$color_cat[dogshelter$Color!="Black/White"&dogshelter$Color!="Tricolor"]<-"NotBWT"
col_tab<-table(dogshelter$color_cat, dogshelter$OutcomeType)
col_tab
chisq.test(col_tab)

# Let's make it look better
library(gmodels)
CrossTable(dogshelter$OutcomeType, dogshelter$color_cat, prop.c = T, prop.r = T,
dnn = c('actual outcome', 'expected outcome'), format=c("SPSS"), chisq = T)

# Exersice
##=========
realestate<-read.csv("realestate.csv", header=T)
CrossTable(realestate$airco, realestate$prefarea, prop.c = T, prop.r = T,
dnn = c('actual a/c', 'expected a/c'), format=c("SPSS"), chisq = T) #p-value = 0.0003; We reject the null. 
##=========

# ANOVA 
#==================================
emp_train<-read.csv("employee_train.csv", header=T)
aggregate(emp_train$productivity ~ emp_train$training, FUN=mean) 
library(dplyr)
emp_train %>% group_by(training) %>% summarize(avg = mean(productivity), std = sd(productivity)) 

# Let's examine a couple of plots 
boxplot(productivity~training, data=emp_train, col=2:4, xlab="Treatment Group") # We get a pretty good picture of "treatment effects"
library(gplots)
plotmeans(emp_train$productivity~emp_train$training, xlab="Training", ylab="Productivity", lwd=3, col="red", p=0.99)

# Now for ANOVA 
emp_train.aov <- aov(productivity~training, data=emp_train)
emp_train.aov
summary(emp_train.aov) # Null rejected. But what intesity level? 

# But we don't know which training. So we use Tukey pairwise comparisons
emp_train.tk<-TukeyHSD(emp_train.aov)
round(emp_train.tk$training,2)

# verify ANOVA assumptions
plot(emp_train.aov)
# Normal distribution assumption 
library(car)
emp_train.fit <- lm(productivity~training, data=emp_train)
qqPlot(emp_train.fit, col="steelblue", pch=16, envelope=T, col.lines=palette()[1])
 
## Exersice 
##=========
 
# one-way ANOVA
# ===============
sales<-read.csv("sales.csv", header=T)
sales %>% group_by(calls) %>% summarize(avg = mean(actual_sales), std = sd(actual_sales)) 
boxplot(actual_sales~calls, data=sales, col=2:7, xlab="Number of Calls") # We get a pretty good picture of "treatment effects"
plotmeans(sales$actual_sales~sales$calls, xlab="Calls", ylab="Sales", lwd=3, col="red", p=0.99)
sales.aov <- aov(actual_sales~calls, data=sales)
sales.aov
summary(sales.aov) # Null rejected. But what number of calls? 

# But we don't know what number of calls, so we use Tukey pairwise comparisons
TukeyHSD(sales.aov) # Oops. We need to convert this into factor 
sales$calls <- as.factor(sales$calls)
sales.aov <- aov(actual_sales~calls, data=sales)
sales.tk <- TukeyHSD(sales.aov)
round(sales.tk$calls,2)
