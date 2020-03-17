# Assignment 1
# by Kenneth Lulie, Data 630 - Ami Gates
# Created 9/30/2018
# Worked on 10/1/2018

#Using the "wages.csv" Data obtained from the course resources.

#objective will be to mine for rules to predict wage.
#Will be reusing code as possible from week 2 and week 3 exercises

#preloading libraries
#assumes packages "arules, arulesviz, TSP, data.table" are already installed
library("arules")
library("arulesViz")

#load in file
setwd("D:/UMUC/630/Week 3/Assignment 1")
wages <- read.csv("wages.csv")


head(wages)

summary(wages)
#data looks really good, looking at the mins and maxs i don't see any obvious areas to correct
#only questionable data is a "1" for wage which appears to be under minimum wage but without knowing
#the actual location and employment status impossible to know for certain.

str(wages)
#need to discretive education, experience, wage, age
#need to factorize sex, union, race, occupation, sector, and marital_status

#534 observations of 11 variables.

#no ID variable to remove.

#Review, no NAs
apply(wages, 2, function (wages) sum(is.na(wages)))
#no Nas returned, very high quality data.


#Review mean of sex
(aggregate(wages[, 6], list(wages$sex), mean))

#review difference by south
(aggregate(wages[, 6], list(wages$south), mean))

(aggregate(wages[, 6], list(wages$union), mean))

boxplot(wages$wage ~ wages$south,  data = wages)
boxplot(wages$wage ~ wages$sex,  data = wages)



#lets start converting to factors using list above
wages$sex <- as.factor(wages$sex)
wages$union <- as.factor(wages$union)
wages$race <- as.factor(wages$race)
wages$occupation<- as.factor(wages$occupation)
wages$sector <- as.factor(wages$sector)
wages$marital_status <- as.factor(wages$marital_status)
str(wages)
#all done no errors

#now to discretize the other variables.
#i think this dataset would be more of general knowledge and understanding than finding hidden connections.
#As such, frequency i think would be better
#for education, i'll just define myself, but all the others seem like frequency will be fine.  
#6 buckets will be fine, seems to be standard and should be enough.  
wages$experience<-discretize(wages$experience, method="frequency",breaks=6)
wages$wage<-discretize(wages$wage, method="frequency",breaks=6)
wages$age<-discretize(wages$age, method="frequency",breaks=6)

#Education is split into less than high school, some college, 4 year degree, post grad.  
wages$education<-discretize(wages$education, method="fixed",breaks=c(0, 11.5, 12, 12.5, 15.5, 16, 16.5, 18.5))

str(wages)

summary(wages$sex)
summary(wages$union)
summary(wages$race)
summary(wages$occupation)
summary(wages$sector)
summary(wages$marital_status)

#interesting distributions



## Model Run #1


rules<-apriori(wages, parameter= list(supp=0.05, conf=0.7, minlen=2), appearance=list(rhs=c("wage=[1,4.5)",
                                                                                            "wage=[4.5,6)", "wage=[6,7.78)",
                                                                                            "wage=[7.78,10)", "wage=[10,13.1)", 
                                                                                            "wage=[13.1,44.5]"), default="lhs"))
rules
#no rules created





## Model Run #2


rules<-apriori(wages, parameter= list(supp=0.02, conf=0.6, minlen=2), appearance=list(rhs=c("wage=[1,4.5)", "wage=[4.5,6)",
                                                                                            "wage=[6,7.78)", "wage=[7.78,10)",
                                                                                            "wage=[10,13.1)", "wage=[13.1,44.5]"), 
                                                                                      default="lhs"))

#sneak peak at the results
rules <- sort(rules, by="lift")
inspect(rules[1:10])
rules

#now we have to prune it.  Using code from the exercise 3
#pruning the returned rules.
#creating a matrix of the subsets.
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
#using the matrix diagnoal, set the ones to the left of the line to false.
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
#stores rules which are redundent
redundant <- colSums(subset.matrix, na.rm=T) >= 1
#remove pruned rules.
rules.pruned <- rules[!redundant]
inspect(rules.pruned)
#only 17 rules were generated after pruning.  The rules all have a high life, by range from 11 to 18 obs which is too low.  
#also confidence is between .6 and .7 which is low.

#plot model 2 rules on a matrix
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))





### Model Run #3

#need to change to 3 buckets for wages instead of 6.
#create new df with original data
origwages <- read.csv("wages.csv")

#change wage in dataset from discretization version to original version
wages$wage<-origwages$wage

#change to 3 buckets here to increase rule count
wages$wage<-discretize(wages$wage, method="frequency",breaks=3)

rules<-apriori(wages, parameter= list(supp=0.04, conf=0.7, minlen=2), appearance=list(rhs=c("wage=[1,6)", "wage=[6,10)", 
                                                                                            "wage=[10,44.5]"), default="lhs"))

#now we have to prune it.  Using code from the exercise 3
#pruning the returned rules.
#creating a matrix of the subsets.
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
#using the matrix diagnoal, set the ones to the left of the line to false.
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
#stores rules which are redundent
redundant <- colSums(subset.matrix, na.rm=T) >= 1
#remove pruned rules.
rules.pruned <- rules[!redundant]

rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)
# 23 rules were generated after pruning.  Lift went down, but support and confidence are much better quality

#plot model 3 rules on a matrix
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))
