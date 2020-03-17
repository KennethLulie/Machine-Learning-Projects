#UMUC 650 Kenneth Lulie Assignment 3
#11-16-2019
#R Exploration of Coachella 2015 Dataset
#Including general EDA and some NLP/Sentiment analysis



##Below is standard code for importing from cloud uploaded dataset for IBM
##Reusing verbatim from sentiment analysis assignment
library(ibmdbR)

dsn_driver <- c("BLUDB")
dsn_database <- c("BLUDB")
dsn_hostname <- c("dashdb-txn-sbox-yp-dal09-03.services.dal.bluemix.net")
dsn_port <- "50000"
dsn_protocol <- "TCPIP"
dsn_uid <- c("LNG59361")
dsn_pwd <- c("srd^xf9qngfv6p1v")

conn_path <- paste(dsn_driver,  
                   ";DATABASE=",dsn_database,
                   ";HOSTNAME=",dsn_hostname,
                   ";PORT=",dsn_port,
                   ";PROTOCOL=",dsn_protocol,
                   ";UID=",dsn_uid,
                   ";PWD=",dsn_pwd,sep="")

mycon <- idaConnect(conn_path) 
idaInit(mycon) 


###This is where we start to diverge.
##Pull in all data by using idaQuery to send an SQL statement to select all from the Coachella dataset
Tweets <- idaQuery("SELECT * from COACHELLA_2015")

#Number of rows is 3846 as expected
nrow(Tweets)

#Number in database is 3846, same as in the DF i just created
idadf(mycon, "SELECT count(*) FROM COACHELLA_2015")

#Tweets shows up in local directory
ls()

#3846 rows, 10 columns again as expected
dim(Tweets)

#Table showed up as expected
idaShowTables()

#Table exists as expected
idaExistTable('COACHELLA_2015')
###Data successfully Imported
###3846 rows, 10 variables same as in csv file

# Start of A ------------------------------------------------------------------------------------------------------
#A a.	Frequency counts and plots that support the purpose of the study


#a1
#Counts of each sentiment, and then pie chart
idadf(mycon, "SELECT coachella_sentiment, count(*)  
      FROM COACHELLA_2015 
      GROUP BY coachella_sentiment")

table(Tweets$COACHELLA_SENTIMENT)
par(mar=c(1,1,1,1))
pie (table(Tweets$COACHELLA_SENTIMENT))


#a2
#Counts of each sentiment by timezone
a2 <- idadf(mycon, "SELECT user_timezone, count(*) as Tweets  
            FROM COACHELLA_2015 
            GROUP BY user_timezone HAVING count(*) > 100")
a2
a2barchart<-ggplot(data=a2, aes(x=a2$USER_TIMEZONE, y=a2$TWEETS)) +
  geom_bar(stat="identity", fill="steelblue")
a2barchart


#a3
#Retweets by each sentiment, and then plot
a3 <- aggregate(Tweets$RETWEET_COUNT, by=list(Category=Tweets$COACHELLA_SENTIMENT), FUN=sum)
a3
library(ggplot2)
library(ggmap)
a3barchart<-ggplot(data=a3, aes(x=a3$Category, y=a3$x)) +
  geom_bar(stat="identity", fill="steelblue")
a3barchart



#a4
#Examine reaction to each of the three Headliners - Jack White, Drake, AC/DC
#a4 -p1 - Drake
a4p1 <- idadf(mycon, "SELECT *  
              FROM COACHELLA_2015 WHERE text like '%drake%' or text like '%Drake%' 
              ")
table(a4p1$COACHELLA_SENTIMENT)
par(mar=c(1,1,1,1))
pie (table(a4p1$COACHELLA_SENTIMENT))

#A4p2 - Jack White
a4p2 <- idadf(mycon, "SELECT *  
              FROM COACHELLA_2015 WHERE text like '%White%' or text like '%white%' 
              ")
table(a4p2$COACHELLA_SENTIMENT)
par(mar=c(1,1,1,1))
pie (table(a4p2$COACHELLA_SENTIMENT))
#A4p3 - AC/DC

a4p3 <- idadf(mycon, "SELECT *  
              FROM COACHELLA_2015 WHERE text like '%AC/DC%' or text like '%ac/dc%' or text like '%acdc%' or text like '%ACDC%' 
              ")
table(a4p3$COACHELLA_SENTIMENT)
par(mar=c(1,1,1,1))
pie (table(a4p3$COACHELLA_SENTIMENT))


# Start of B --------------------------------------------------------------------------------
#B
#b.	Run 2-3 methods from the following list.  
#For each method, discuss the rationale, pre-processing, input parameters you used, and interpret the results.


##### B1 Trend Analysis
#Load Library
library (dplyr)

#Create new dataframe with an SQL query to only pull dates and sentiment
drs <- idadf(mycon, "SELECT TWEET_CREATED, COACHELLA_SENTIMENT  FROM COACHELLA_2015")

#Update tweet created column to new data frame that will plot well
drs$TWEET_CREATED<- as.Date(drs$TWEET_CREATED, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"),optional = FALSE)

#Group by each date
ByDateBySent <- drs %>% group_by(COACHELLA_SENTIMENT,TWEET_CREATED) %>% dplyr::summarise(count = n())

#Create plot
ByDateBySentPlot = ggplot() + geom_line(data=ByDateBySent, aes(x=TWEET_CREATED, y=count, group =COACHELLA_SENTIMENT , color=COACHELLA_SENTIMENT)) 

#Review Plot
ByDateBySentPlot


#### Text Mining Preprocessing

install.packages("SnowballC")

library("tm")
library("wordcloud")
library ("SnowballC")

positive <- idadf(mycon, "SELECT   TEXT 
                  FROM  COACHELLA_2015 
                  WHERE COACHELLA_SENTIMENT='positive'")

docs<-VectorSource(positive$TEXT)
docs<-Corpus(docs)

docs <- tm_map(docs, stripWhitespace)

inspect(docs[[1]])
inspect(docs[[2]])
inspect(docs[[20]])

#Remove invalid characters
removeInvalid<-function(x) gsub("[^\x01-\x7F]", "", x)
docs <- tm_map(docs, content_transformer(removeInvalid))

#Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

#Remove punctuation
docs <- tm_map(docs, removePunctuation)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")   #Remove @
docs <- tm_map(docs, toSpace, "/")   #Remove /
docs <- tm_map(docs, toSpace, "\\|") #Remove |

#remove the numbers
docs <- tm_map(docs, removeNumbers)

#Make it lowercase
docs <- tm_map(docs, tolower)

#Remove stopwords from english and SMART lists
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))

#Restrip whitespace
docs <- tm_map(docs, stripWhitespace)

#Stem the words
docs <- tm_map(docs, stemDocument)

#Create document term matrix
dtm <- DocumentTermMatrix(docs)

m <- as.matrix(dtm)   #Convert dtm to a matrix
dim(m)                       # Display number of terms and number of documents
View(m[1:50, 1:50])




####B2 - Word Cloud
dtms <- removeSparseTerms(dtm, 0.6) # Prepare the data (max 60% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq=35, max.words=100, rot.per=0.2, scale=c(0.9, 0.9), colors=dark2)




###B3 Dendogram
dtms <- removeSparseTerms(dtm, 0.98)
d <- dist(t(dtms), method="euclidian")
fit <- hclust(d=d, method="ward.D2")
plot(fit, hang=-1)




#### Start of C -------------------------------------------------------------------------------- 
#c.	Be creative and run analysis beyond examples in the walk through.  


##C1 Provide Frequent Terms to Social Media Team

c1 <- findFreqTerms(dtm, lowfreq=50)
c1

##C2 What are people excited about
c2 <- findAssocs(dtm, c("band", "good", "ticket", "lineup"), corlimit=0.15)
c2


##C3 What are the biggest tweets?
c3 <- idadf(mycon, "SELECT NAME, COACHELLA_SENTIMENT, RETWEET_COUNT, TEXT  
                          FROM COACHELLA_2015 WHERE RETWEET_COUNT >= 50
                  ")
c3
##C4 Expanded Dendogram
dtms <- removeSparseTerms(dtm, 0.99)
d <- dist(t(dtms), method="euclidian")
fit <- hclust(d=d, method="ward.D2")
plot(fit, hang=-1)



####C5 - Expanded Word Cloud
dtms <- removeSparseTerms(dtm, 0.8) # Prepare the data (max 60% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq=20, max.words=100, rot.per=0.2, scale=c(0.9, 0.9), colors=dark2)





