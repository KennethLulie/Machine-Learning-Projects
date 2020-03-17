# Assignment 2
# by Kenneth Lulie, Data 630 - Ami Gates
# Created 10/12/2018
# worked on 10/13/2018

##### Data preprocessing

##Load libraries in, as specified in the Naive Bayes Script provided
#load the arules and e1071 library into memory
#need to do this each time you start the new R session
library("arules")
library("e1071")

#Read the data into a data frame
origglow <- read.csv("D:/UMUC/630/Week 5/Assignment 2/glow500.csv")

glow<-origglow

#review data
str(glow)
summary(glow)

#data seems good, no problems detected


#this function shows results verifying the info given.
apply(glow, 2, function (glow) sum(is.na(glow)))
#no NAS are detected



summary(glow)



### Removal of variables
#remove the unique identifier for specific people
glow$SUB_ID<-NULL

#remove the unique identifier for doctor, simply too many
glow$PHY_ID<-NULL



#as we have no information about the six sites provided, we will also remove this.
glow$SITE_ID<-NULL

#Fracscore is calculated off of age, priorfrac, momfrac, weight, armassist, and smoke
#these are already included in our model (weight in is with BMI)
#we need to remove, naive bayes works best with no correlation between variables
glow$FRACSCORE<-NULL

#remove weight and height, naive bayes assumes lack of correlation between independent variables
#additionally, the relationship between this data is in the BMI variable
glow$HEIGHT<-NULL
glow$WEIGHT<-NULL




### CONVERT TO FACTORS
glow$PRIORFRAC<- as.factor(glow$PRIORFRAC)
glow$PREMENO<- as.factor(glow$PREMENO)
glow$MOMFRAC<- as.factor(glow$MOMFRAC)
glow$ARMASSIST<- as.factor(glow$ARMASSIST)
glow$SMOKE<- as.factor(glow$SMOKE)
glow$RATERISK<- as.factor(glow$RATERISK)
glow$FRACTURE<- as.factor(glow$FRACTURE)


### Boxplot

boxplot(glow$AGE,  data = glow)
boxplot(glow$BMI,  data = glow)
boxplot(glow$AGE, glow$BMI, col="maroon",names=c("Age at Enrollment","BMI at Enrollment"))




###Discretization
#We will need to discretize age and BMI
#Naive bayes requires discretization of continous variables

#discretize age
glow$AGE<-discretize(glow$AGE, "frequency", categories=6)
summary(glow$AGE)

#discretize the BMI variable
glow$BMI<-discretize(glow$BMI, "frequency", categories=6)
summary(glow$BMI)










##### Model building

#make sure that the result is reproducible
set.seed(1234)


#Model 1

#split the data into a training and test set
ind <- sample(2, nrow(glow), replace = TRUE, prob = c(0.7, 0.3))
train.data <- glow[ind == 1, ]
test.data <- glow[ind == 2, ]


#build the model and store in a variable model
model<-naiveBayes(FRACTURE~., train.data)
#output the model
model

#confusion matrix for the training set; need to round the estimated values
table(predict(model, train.data), train.data$FRACTURE)
#confusion matrix for the test data
table(predict(model, test.data), test.data$FRACTURE)

#mosaic plot
mosaicplot(table(predict(model, test.data), test.data$FRACTURE), shade=TRUE, main="Predicted vs. Actual FRACTURES")




###Model 2

##Model 2 will reuse fracscore to see if it assists

#Re-add Fracture score
glow <- cbind(glow, FRACSCORE = origglow$FRACSCORE)

glow$FRACSCORE<-discretize(glow$FRACSCORE, "frequency", categories=5)

ind <- sample(2, nrow(glow), replace = TRUE, prob = c(0.7, 0.3))
train.data <- glow[ind == 1, ]
test.data <- glow[ind == 2, ]


#build the model and store in a variable model
model<-naiveBayes(FRACTURE~., train.data)
#output the model
model

#confusion matrix for the training set; need to round the estimated values
table(predict(model, train.data), train.data$FRACTURE)
#confusion matrix for the test data
table(predict(model, test.data), test.data$FRACTURE)

#mosaic plot
mosaicplot(table(predict(model, test.data), test.data$FRACTURE), shade=TRUE, main="Predicted vs. Actual FRACTURES")



####Model 3

#remove fracscore, rediscretize AGE and BMI with 8 ranges
#remove fracscore
glow$FRACSCORE<-NULL

#Re add age and BMI
glow$AGE <- origglow$AGE
glow$BMI <- origglow$BMI



#discretize age
glow$AGE<-discretize(glow$AGE, "cluster", categories=5)
summary(glow$AGE)

#discretize the BMI variable
glow$BMI<-discretize(glow$BMI, "cluster", categories=5)
summary(glow$BMI)


ind <- sample(2, nrow(glow), replace = TRUE, prob = c(0.7, 0.3))
train.data <- glow[ind == 1, ]
test.data <- glow[ind == 2, ]


#build the model and store in a variable model
model<-naiveBayes(FRACTURE~., train.data)
#output the model
model

#confusion matrix for the training set; need to round the estimated values
table(predict(model, train.data), train.data$FRACTURE)
#confusion matrix for the test data
table(predict(model, test.data), test.data$FRACTURE)

#mosaic plot
mosaicplot(table(predict(model, test.data), test.data$FRACTURE), shade=TRUE, main="Predicted vs. Actual FRACTURES")

