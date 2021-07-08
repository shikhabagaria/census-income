tweets=read.csv("tweets.csv")
str(tweets)
tweets$Negative= factor(tweets$Avg<=-1)
str(tweets)
table(tweets$Negative)
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
corpus=Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus[[1]]$content
corpus=tm_map(corpus, tolower)
corpus[[1]]$content
corpus=tm_map(corpus, removePunctuation)
corpus[[1]]$content
corpus=tm_map(corpus, removeWords, c=("apple", stopwords("english")))
corpus=tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus=tm_map(corpus, stemDocument)
frequencies=DocumentTermMatrix(corpus)
frequencies
sparse=removeSparseTerms(frequencies, 0.98)
sparse
tweetsSparse=as.data.frame(as.matrix(sparse))
sparse=removeSparseTerms(frequencies, 0.995)
sparse
tweetsSparse=as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
tweetsSparse$Negative=tweets$Negative
library(caTools)
set.seed(123)
split=sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse=subset(tweetsSparse, split==TRUE)
testSparse=subset(tweetsSparse, split==FALSE)
libarry(rpart)
library(rpart.plot)
library(rpart)
library(rpart.plot)
tweetcart=rpart(Negative ~., data=trainSparse)
tweetcart=rpart(Negative ~., data=trainSparse, method="class")
prp(tweetcart)
predcart=predict(tweetcart, newdata=testSparse, type="class")
table(testSparse$Negative, predcart)
table(testSparse$Negative)
300/355
library(randomForest)
set.seed(123)
tweetrf=randomForest(Negative ~., data= trainSparse)
predictrf=predict(tweetrf, newdata=testSparse)
table(testSparse$Negative, predictrf)
       predictrf
        FALSE TRUE
  FALSE   293    7
  TRUE     33   22
(293+22)/(293+22+7+33)