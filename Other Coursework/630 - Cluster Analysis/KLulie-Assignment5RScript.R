# Assignment 5
# by Kenneth Lulie, Data 630 - Ami Gates
# Created 11/16
#Worked on 11/17

#Set random seed so it is reproducible
set.seed(1234)

#Load cluster package
library("cluster")
#load dplyr to assist in column removal
library(dplyr)

#Read the CSV file. and set working directory
setwd("D:/UMUC/630/Week 11/Assignment 5")
origwine <- read.csv("winequality_red.csv")


#Set up a DF to work off of
wine <- origwine

#we can see that the vast majority of the observations are in 5 6 and 7.  We will move all 3s and 4s to 5 and all 8s to 7s.
#and then have low quality, medium quality, high quality.
summary(origwine)

# data looks fine.
summary(wine)

#Need to remove quality before clustering
wine <- select(wine, -quality)

#Scale variable before using in clustering
wine <- scale(wine)

# data looks fine.
summary(wine)





### Data exploration
counts <- table(origwine$quality)
barplot(counts, main="Wine Quality Distribution", 
        xlab="Wine Quality")




#set quality to factor in original
#Transform all to 3 groups
origwine$quality[origwine$quality == 3] <- 5
origwine$quality[origwine$quality == 4] <- 5
origwine$quality[origwine$quality == 8] <- 7







######## Model One - 3 clusters



#Run the method and store the result in kc variable
kc1<-kmeans(wine, 3, nstart = 10)



#Cluster to class evaluation
table(origwine$quality, kc1$cluster)
#Do cluster plot
clusplot(wine, kc1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

print(kc1)
kc1$betweenss
kc1$iter


######## Model Two - 5 clusters



#Run the method and store the result in kc variable
kc2<-kmeans(wine, 5, nstart = 10)



#Cluster to class evaluation
table(origwine$quality, kc2$cluster)

#Do cluster plot
clusplot(wine, kc2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


print(kc2)
kc2$betweenss
kc2$iter  





######## Model Three - 7 clusters



#Run the method and store the result in kc variable
kc3<-kmeans(wine, 7, nstart = 10)


#Cluster to class evaluation
table(origwine$qual, kc3$cluster)

#Do cluster plot
clusplot(wine, kc3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)



print(kc3)
kc3$betweenss
kc3$iter






### Centroids 3 and 4 evaluation
## Create new DF, add kc3 cluster results
C <- origwine
C$index <- kc3$cluster


##Create DF with only objects where the cluster assigned was 3
#careful on capitalization here
C3 <- subset(C, index == 3)
#use summary to get means.
summary (C3)
##Repeat above for 4

C4 <- subset(C, index == 4)
summary (C4)











