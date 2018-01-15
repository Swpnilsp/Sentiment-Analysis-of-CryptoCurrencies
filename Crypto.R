library(twitteR)
library(dplyr)
library(tm )
library(wordcloud)
library(tidytext)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggthemes)


## bitcoin, ripple,Ethereum 
## 
consumer_key<-'xxxyyyzzz'
consumer_secret<-'xxxyyyzzz'
access_token<-'xxxyyyzzz'
access_token_secret<-'xxxyyyzzz'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)
bitcoin = twitteR::searchTwitter('#bitcoin -filter:retweets',lang = "en", n = 5000, since = '2017-12-24',
                            until = '2017-12-31',retryOnRateLimit = 1)
d = twitteR::twListToDF(bitcoin)
write.csv(d,"bitcoin.csv")

ripple = twitteR::searchTwitter('#ripple -filter:retweets',lang = "en", n = 5000, since = '2017-12-24',
                               until = '2017-12-31',retryOnRateLimit = 1)
d = twitteR::twListToDF(ripple)
write.csv(d,"ripple.csv")

ethereum = twitteR::searchTwitter('#ethereum -filter:retweets',lang = "en", n = 5000, since = '2017-12-24',
                                until = '2017-12-31',retryOnRateLimit = 1)
d = twitteR::twListToDF(ethereum)
write.csv(d,"ethereum.csv")


bitcoin<-read.csv("bitcoin.csv",fileEncoding="latin1")
ripple<-read.csv("ripple.csv",fileEncoding="latin1")
ethereum<-read.csv("ethereum.csv",fileEncoding="latin1")



# for now, we will just concentrate on the text of tweets
bitcoin<-bitcoin$text
ripple<-ripple$text
ethereum<-ethereum$text
bitcoin<-as.character(bitcoin)
ripple<-as.character(ripple)
ethereum<-as.character(ethereum)


# Text tranformations- removing whitespaces, commong stop words,punctuations 

bitcoin<-stripWhitespace(bitcoin)
## this will remove all other characters except $ sign
bitcoin<-gsub("[^[:alnum:][:space:]$]", "", bitcoin)
bitcoin<-tolower(bitcoin)
bitcoin<-removeWords(bitcoin, c(stopwords("english"),'ampamp','retweet','just','comment','amp','bitcoin','btc','crypto','cryptocurrency'))


ripple<-stripWhitespace(ripple)
## this will remove all other characters except $ sign
ripple<-gsub("[^[:alnum:][:space:]$]", "", ripple)
ripple<-tolower(ripple)
ripple<-removeWords(ripple, c(stopwords("english"),'ampamp','retweet','just','comment','amp','ripple','xrp','$xrp','crypto'))


ethereum<-stripWhitespace(ethereum)
## this will remove all other characters except $ sign
ethereum<-gsub("[^[:alnum:][:space:]$]", "", ethereum)
ethereum<-tolower(ethereum)
ethereum<-removeWords(ethereum, c(stopwords("english"),'ampamp','retweet','just','comment','amp','ethereum','cryptocurrency','xrp'))

#note8<-unlist(strsplit(note8, split = ' '))

# converting to vector
bitcoinTweets<-VectorSource(bitcoin)
rippleTweets<-VectorSource(ripple)
ethereumTweets<-VectorSource(ethereum)

# converting verctor source to Volatile Corpus, which is a nested list ($content and $meta). This helps extracting individual tweets 
# for example, bitcoinTweets[[20]][1] will retreive 20th tweet and bitcoinTweets[[20]][2] will retreive metadata of 20th

bitcoinTweets<-VCorpus(bitcoinTweets)
rippleTweets<-VCorpus(rippleTweets)
ethereumTweets<-VCorpus(ethereumTweets)


# Creating document term  matrix
bitcoin_dtm<-DocumentTermMatrix(bitcoinTweets)
ripple_dtm<-DocumentTermMatrix(rippleTweets)
ethereum_dtm<-DocumentTermMatrix(ethereumTweets)

bitcoin_m<-as.matrix(bitcoin_dtm)
ripple_m<-as.matrix(ripple_dtm)
ethereum_m<-as.matrix(ethereum_dtm)

dim(bitcoin_m)
dim(ripple_m)
dim(ethereum_m)

## getting the word frequencies
bitcoin_wf<-colSums(bitcoin_m)
bitcoin_wf<-sort(bitcoin_wf,decreasing = TRUE)
ripple_wf<-colSums(ripple_m)
ripple_wf<-sort(ripple_wf,decreasing = TRUE)
ethereum_wf<-colSums(ethereum_m)
ethereum_wf<-sort(ethereum_wf,decreasing = TRUE)

#Plotting most frequent words
barplot(bitcoin_wf[1:20],col='red',las=2,main = 'Bitcoin')
barplot(ripple_wf[1:20],col='red',las=2,main = 'Ripple')
barplot(ethereum_wf[1:20],col='red',las=2,main = 'Ethereum')

wordcloud(names(bitcoin_wf),bitcoin_wf,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 100)
wordcloud(names(ripple_wf),ripple_wf,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 100)
wordcloud(names(ethereum_wf),ethereum_wf,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 100)


## common words in both iphonex and note8 related tweets
bitcoin<-read.csv("bitcoin.csv",fileEncoding="latin1")
ripple<-read.csv("ripple.csv",fileEncoding="latin1")
ethereum<-read.csv("ethereum.csv",fileEncoding="latin1")

bitcoin<-paste(bitcoin$text,collapse = '')
ripple<-paste(ripple$text,collapse = '')
ethereum<-paste(ethereum$text,collapse = '')

all_tweets<-c(bitcoin,ripple,ethereum)
all_tweets<-stripWhitespace(all_tweets)
all_tweets<-gsub("[^[:alnum:][:space:]$]", "", all_tweets)
all_tweets<-tolower(all_tweets)
all_tweets<-removeWords(all_tweets, c(stopwords("english"),'crypto','cryptocurrency','xrp','ampamp','amp'))

all_tweets<-VectorSource(all_tweets)
all_tweets<-VCorpus(all_tweets)

all_tweets_tdm<-TermDocumentMatrix(all_tweets)
all_tweets_m<-as.matrix(all_tweets_tdm)
commonality.cloud(all_tweets_m,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 50)


## Sentiment analysis
bitcoin<-read.csv("bitcoin.csv",fileEncoding="latin1")
ripple<-read.csv("ripple.csv",fileEncoding="latin1")
ethereum<-read.csv("ethereum.csv",fileEncoding="latin1")

bitcoin<-bitcoin$text
ripple<-ripple$text
ethereum<-ethereum$text
bitcoin<-as.character(bitcoin)
ripple<-as.character(ripple)
ethereum<-as.character(ethereum)

bitcoin<-as.tibble(bitcoin)
ripple<-as.tibble(ripple)
ethereum<-as.tibble(ethereum)

#Separating the words
bitcoin<-bitcoin%>%unnest_tokens(word, value)
ripple<-ripple%>%unnest_tokens(word, value)
ethereum<-ethereum%>%unnest_tokens(word, value)

#Counting the sentiment related words for bitcoin
bitcoinSentiments<-sqldf('select sentiments.*,bitcoin.word as SentimentWord from sentiments, bitcoin
                         where sentiments.word=bitcoin.word')

bitcoinSentiments<-bitcoinSentiments%>%group_by(sentiment)%>%summarise(count=n())
bitcoinSentiments<-bitcoinSentiments%>%arrange(desc(count))

#Counting the sentiment related words for ripple
rippleSentiments<-sqldf('select sentiments.*,ripple.word as SentimentWord from sentiments, ripple
                         where sentiments.word=ripple.word')

rippleSentiments<-rippleSentiments%>%group_by(sentiment)%>%summarise(count=n())
rippleSentiments<-rippleSentiments%>%arrange(desc(count))

#Counting the sentiment related words for ethereum
ethereumSentiments<-sqldf('select sentiments.*,ethereum.word as SentimentWord from sentiments, ethereum
                         where sentiments.word=ethereum.word')

ethereumSentiments<-ethereumSentiments%>%group_by(sentiment)%>%summarise(count=n())
ethereumSentiments<-ethereumSentiments%>%sort(desc(count))



ggplot(data =filter(na.omit(bitcoinSentiments)),aes(x = reorder(sentiment,count),y =count))+geom_bar(stat='identity')+
  theme_solarized_2(light = FALSE)+coord_flip()+labs(title='Bitcoin Sentiments',x='Sentiments',y='Sentiment Count')

ggplot(data =filter(na.omit(rippleSentiments)),aes(x = reorder(sentiment,count),y =count))+geom_bar(stat='identity')+
  theme_solarized_2(light = FALSE)+coord_flip()+labs(title='Ripple Sentiments',x='Sentiments',y='Sentiment Count')

ggplot(data =filter(na.omit(ethereumSentiments)),aes(x = reorder(sentiment,count),y =count))+geom_bar(stat='identity')+
  theme_solarized_2(light = FALSE)+coord_flip()+labs(title='Ethereum Sentiments',x='Sentiments',y='Sentiment Count')
  

wordcloud(bitcoinSentiments$sentiment,bitcoinSentiments$count,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 15)
wordcloud(rippleSentiments$sentiment,rippleSentiments$count,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 15)
wordcloud(ethereumSentiments$sentiment,ethereumSentiments$count,colors = c("grey80", "darkgoldenrod1", "tomato"),max.words = 15)

