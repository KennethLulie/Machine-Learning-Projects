# Assignment 3
# by Kenneth Lulie, Data 630 - Ami Gates
# Created 10/22/2018
#Worked on 10/26/2018 through 10/28/2018

###Initial commands to set up session


#Standard introductory screenshots
Sys.time()
Sys.info()
R.version

# load libraries, load data
library("party")

#Set Working DIrecty
setwd("D:/UMUC/630/Week 7/")


#load in german credit file
credit<-read.csv(file="credit-g.csv", head=TRUE, sep=",")

#preview the structure
str(credit) 

#Don't see any values or data types that need changing

#Change bad to 0 and good to 1 for ROC curve later
#Converts to char from factor, then sets bad to 0 and good to 1, then sets it to num from char
credit$class <- as.character(credit$class)
credit$class[credit$class == "bad"] <- 0
credit$class[credit$class == "good"] <- 1
credit$class <- as.numeric(credit$class)





#Check data quality, dont see any NAs in the data
apply(credit, 2, function (credit) sum(is.na(credit)))

#Review data with summary
#No obvious problems, good 1000 points in dataset which is good.
summary(credit)



###Make boxplots
boxplot(credit$credit_amount, col="maroon")

boxplot(credit$age, credit$duration, col="maroon",names=c("Age in Years","Duration of Loan in Months"))




### reused commands from exercise, standard 70 30% split
#split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(credit), replace = TRUE, prob = c(0.7, 0.3))
train.data <- credit[ind == 1, ]
test.data <- credit[ind == 2, ]



## reused commands from excercise, predit on class and use all variables.
#6. Run the method on a training data
myFormula<-class~.
credit_ctree <- ctree(myFormula, data = train.data)

##Reuse commands to print the tree structure
#7. output the tree structure
print(credit_ctree) 


#8. visualize the tree
nodes(credit_ctree, 2)
plot(credit_ctree)
plot(credit_ctree, type="simple")


#10. Evaluate the model on a test data
testPred <- predict(credit_ctree, newdata = test.data)



### Get AUC number
library(pROC)
auc(test.data$class, testPred)
#.7498



### plot AUC Chart
library(ROCR)

# explanation of order prediction(predictions, labels, label.ordering = NULL)
#so you want to create a object with the prediction data called pred.
#The first agrument hte prediction takes, is the continous numbers generated for the predictions by the ctree
#The second agrument is the label or 'the truth'.
pred <- prediction(testPred, test.data$class, label.ordering = NULL)

#Plot it out, measure tpr, xmeasure fpr
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b= 1)

## I believe this is indicating that we can get 50% of the true positives by using a 20% false positive cutoff.
## we can get about 75% of the true positive, by allowing a 40% false positive cutoff. 




## Confuson Matrix with a normal 50% split.
table (round(testPred), test.data$class)
# get spec to sens ratio
#    0   1
#0  57  34
#1  51 160
#-51*5 + -34 = -289


## This is a only 75% and up cutoff
strictcutoff <- testPred
strictcutoff[strictcutoff<.75] <- 0
strictcutoff[strictcutoff >.75] <- 1
table (strictcutoff, test.data$class)
##I think top is actual, and side is predictions
# so for this there would be 15 predicted as good, but actually bad, and 96 predicted bad but actually good
#    0  1
#0 93 96
#1 15 98

#-15*5 + -96
#-171


##This model uses a 35% and up cutoff
relaxcutoff <- testPred
relaxcutoff[relaxcutoff <.35] <- 0
relaxcutoff[relaxcutoff >.35] <- 1

table (relaxcutoff, test.data$class)
#    0   1
#0   9   2
#1  99 192
# -99 * 5 + -2 = -497

