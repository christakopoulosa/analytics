##Add relevant libraries
library(xts)

##Delete first two lines
X417016 <- X417016[-c(1,2),]

##Make dataframe .xts file
data <- xts(X417016[,c(1,2,9,10,11,12,13)], order.by = as.POSIXct(X417016$Ended, format = c("%d/%m/%Y %H:%M:%OS")))
head(data, 5)

##Create data frame from the xts object 'data'
dataAsDataFrame <- data.frame(date=index(data), coredata(data[,c(3,4,5,6,7)]))

##Clear text from common words
library(tm)
dataAsDataFrame$characterVector <- as.character(dataAsDataFrame$Q2..How.could.we.improve.this.service.)
dataAsDataFrame$characterVector2 <- as.character(dataAsDataFrame$Q1..Overall..how.did.you.feel.about.the.service.you.received.today.)
dataAsDataFrame$characterVector3 <- as.character(dataAsDataFrame$Q3..What.Licensing.task.did.you.need.to.carry.out.today.)
dataAsDataFrame$characterVector4 <- as.character(dataAsDataFrame$Q4..How.easy.was.it.to.complete.this.task.)
dataAsDataFrame$characterVector5 <- as.character(dataAsDataFrame$Q5..If.you.found.it.fairly.or.extremely.difficult.to.complete.this.task..please.could.you.say.why.this.was.)

head(dataAsDataFrame$characterVector, 5)
 
corpus <- Corpus(VectorSource(dataAsDataFrame$characterVector))
corpus[[1]]$content

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content


##Add snowball library
library(SnowballC)
corpus <- Corpus(VectorSource(corpus))

##Create wordcloud
library(wordcloud)
wordcloud(words = corpus,
          max.words = 100,
          ordered.colors = FALSE,
          colors = brewer.pal(5, "Dark2")[factor(dataAsDataFrame$Q1..Overall..how.did.you.feel.about.the.service.you.received.today.)])


