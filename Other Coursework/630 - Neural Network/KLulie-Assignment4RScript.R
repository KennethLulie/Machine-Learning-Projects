# Assignment 4
# by Kenneth Lulie, Data 630 - Ami Gates
# Created 11/4
#Work on 11/5 through 11/11


#Standard introductory screenshots
Sys.time()
Sys.info()
R.version

#load the library to use later.
library("neuralnet")

#Set Working DIrecty
setwd("D:/UMUC/630/Week 9/Assignment 4")

#import data
heart<-read.csv(file="whas1.csv", head=TRUE, sep=",")







#Run the summary command 
summary(heart)
#check also with str
str(heart)



#arrays start at 1 in python
#Remove ID, unique identifier
heart<-heart[2:14]

##Dstat is the initial discharge, LENFOL is the amount of time that passed from INITIAL admission to follow up admission.
#Therefore, its a more rewarding question to ask if they will survive the initial admission
#The purpose will therefore be to make a model that can help doctors on initial admission identify the
#likely hood of death from initial discharge.

#For this purpose, we will remove all data that doctors would not know after running tests on initial admissions.
#Or would not be relevant.
#Therefore we will remove, the length of the stay, the Length of follow up time, the follow up discharge (FSTAT)
#and Year and YRGRP as we want this to be more useful on a present day basis.
#This leaves AGE, SEX, CPK, SHO, CHF, MIORD,MITYPE to predict off of.

#remove remaining variables
heart <- subset(heart, select = -c(YEAR, YRGRP, LENSTAY, LENFOL, FSTAT))


#Make boxplot
boxplot(heart$AGE, col="maroon")

#Scale down all numeric before running neural network
#do not need to scale categorical
heart$AGE<-scale(heart$AGE)
heart$CPK<-scale(heart$CPK)

#Scaling looks appropriate.
summary(heart)

#However, MITYPE has 3 levels and is not ordinal.  
#1 is Q-Wave, 2 is not Q-wave, and 3 is indeterminate.
#We will therefore make 2 new variables, QMITYPE and QMITYPEN.  If both are 0, it indicates the third level.  
#We will then remove MITYPE


#Create new variables, default 0
heart$MIQ <- 0
heart$MINQ <- 0

#assign values to these new variables based on MITYPE
heart$MIQ[heart$MITYPE == 1] <- 1
heart$MINQ[heart$MITYPE == 2] <- 1

#Remove MITYPE
heart <- subset(heart, select = -c(MITYPE))




#add DSTAT2 to end, remove DSTAT, change DSTAT2 to DSTAT
#to make the other formulas that rely on it being the end work.
heart$DSTAT2 <-heart$DSTAT
heart <- subset(heart, select = -c(DSTAT))
heart$DSTAT <-heart$DSTAT2
heart <- subset(heart, select = -c(DSTAT2))



#make sure that the result is reproducible
set.seed(12345)
#split the data into a training and test set
ind <- sample(2, nrow(heart), replace = TRUE, prob = c(0.7, 0.3))
train.data <- heart[ind == 1, ]
test.data <- heart[ind == 2, ]


##### Start of NN 1



#Build the model. 
##1 hidden layer, with 2 nodes.
nn<-neuralnet(formula = DSTAT~AGE+SEX+CPK+SHO+CHF+MIORD+MIQ+MINQ,
              data = train.data, hidden=2, err.fct="ce", linear.output = FALSE)


#names command displays the available neural network properties
names(nn)

#a matrix containing the reached threshold, needed steps, 
#error, AIC and BIC (if computed) and weights for every repetition. Each heart represents one repetition.
#Weights and biases
nn$result.matrix


#Plot showing the NN
plot(nn)

#Compute values against train data and round
mypredict1 <- compute(nn, train.data[, 1:8])$net.result
mypredict1 <-apply(mypredict1, c(1), round)

#Build the confusion matrix
table(mypredict1, train.data$DSTAT, dnn =c("Predicted", "Actual"))
#Accuracy
mean(mypredict1==train.data$DSTAT)
#90.158
#           Actual
#Predicted   0   1
#        0 268  31
#         1   0  16



##Run on the test data, use all variables except the dependent.
testPred1 <- compute(nn, test.data[, 1:8])$net.result
testPred1<-apply(testPred1, c(1), round)
#make confusion matrix
table(testPred1, test.data$DSTAT, dnn =c("Predicted", "Actual"))
#check accuracy
mean(testPred1==test.data$DSTAT)
#test accuracy, 84.337
#         Actual
#Predicted   0   1
#         0 129  24
#        1   2  11








##### Start of NN 2



#Build the model. 
#using 2 hidden layers, with 3 nodes each.
nn2<-neuralnet(formula = DSTAT~AGE+SEX+CPK+SHO+CHF+MIORD+MIQ+MINQ, 
              data = train.data, hidden=c(2,2), err.fct="ce",  linear.output = FALSE, stepmax = 1000000)
## will take a while


#make plot of second model.
plot(nn2)


#Weights and biases
nn2$result.matrix



#Run on train data
mypredict2<-compute(nn2, nn$covariate)$net.result
mypredict2<-apply(mypredict2, c(1), round)

#Build the confusion matrix
table(mypredict2, train.data$DSTAT, dnn =c("Predicted", "Actual"))
mean(mypredict2==train.data$DSTAT)
#92.3
#           Actual
#Predicted   0   1
#         0 261  17
#         1   7  30



#Run again on test data.
testPred2 <- compute(nn2, test.data[, 1:8])$net.result
testPred2<-apply(testPred2, c(1), round)
#Make table.
table(testPred2, test.data$DSTAT, dnn =c("Predicted", "Actual"))
#Check accuracy
mean(testPred2==test.data$DSTAT)
#.84337

#           Actual
#Predicted   0   1
#         0 121  16
#         1  10  19





####### Model 3 


#Build the model. 
#using 2 hidden layers, with 5 nodes each.
## We will remove 
nn3<-neuralnet(formula = DSTAT~AGE+SEX+CPK+SHO+CHF+MIORD+MIQ+MINQ, 
              data = train.data, hidden=c(5 , 5), err.fct="ce",  linear.output = FALSE, stepmax = 1000000)
### will take a long to run, aprox 5 minutes on my machine


#make plot of second model.
plot(nn3)


#Run on train data
mypredict3<-compute(nn3, nn$covariate)$net.result
mypredict3<-apply(mypredict3, c(1), round)

#Build the confusion matrix
table(mypredict3, train.data$DSTAT, dnn =c("Predicted", "Actual"))
mean(mypredict3==train.data$DSTAT)

#1
#Actual
#Predicted   0   1
#         0  268   0
#         1   0   47




#Weights and biases
nn3$result.matrix

#Run again on test data.
testPred3 <- compute(nn3, test.data[, 1:8])$net.result
testPred3<-apply(testPred3, c(1), round)
#Make table.
table(testPred3, test.data$DSTAT, dnn =c("Predicted", "Actual"))
#Check accuracy
mean(testPred3==test.data$DSTAT)
#.819


#           Actual
#Predicted   0   1
#          0 113  12
#          1  18  23
