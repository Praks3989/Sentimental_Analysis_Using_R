#SPL Project
#Team Members -
  #Gaurav Dhamdhere
  #Devdatta Kulkarni
  #Prakash Patel
  #Pawan Araballi
  #Venkat Hitendra Babu Narla

#Sentiment analysis of Twitter tweets

#Uncomment follwoing Package install commands if running for the first time


#install.packages("jsonlite")
#install.packages("plyr")
#install.packages("stringr")
#install.packages("e1071")
#install.packages("data.table")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("MASS")
#install.packages("SnowballC")
#install.packages("plotrix")
#install.packages("tm")

# Load required libraries
library(jsonlite)
library(plyr)
library(stringr)
library(e1071)
library(data.table)
library(tm)
library(wordcloud)
library(MASS)
library(SnowballC)
library(plotrix)
library(tm)

#Removing garbage values
remove(a,a1,a2)
remove(x,tweet_scores,score)
remove(x1,x2,x3,x4)
remove(row1,row2,row3,row4,y)
remove(tweets,SentimentDictionary,worst,bad,good,best,split_tweets,term,tweet_file)
remove(bestmatch,goodmatch,worstmatch,badmatch,sentiment)
remove(tweet_text,tweet_text1,wc1,wc2,wc3,wc4,wc_final)
 
  
tweet_file <- fromJSON("U:/Study/SPL/Project/tweets_data.json",simplifyDataFrame = TRUE)
  

tweets<- paste(tweet_file$text,tweet_file$description,sep = " ")  
#tweets  
  
  SentimentDictionary<-read.delim(file='U:/Study/SPL/Project/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
  
  names(SentimentDictionary) <- c('word', 'score_of_sentiments')
  
  #Convert all words in the dictionary to lower case
  SentimentDictionary$word<- tolower(SentimentDictionary$word)
  
  #Cluster the Dictionary words into worst, bad, good, best  
  worst<- c(SentimentDictionary$word[SentimentDictionary$score_of_sentiments==-5 | SentimentDictionary$score_of_sentiments==-4 | SentimentDictionary$score_of_sentiments==-3])
  bad<-c(SentimentDictionary$word[SentimentDictionary$score_of_sentiments==-2 | SentimentDictionary$score_of_sentiments==-1])
  good<-c(SentimentDictionary$word[SentimentDictionary$score_of_sentiments==2 | SentimentDictionary$score_of_sentiments==1])
  best<-c(SentimentDictionary$word[SentimentDictionary$score_of_sentiments==5 | SentimentDictionary$score_of_sentiments==4 | SentimentDictionary$score_of_sentiments==3]) 
  
  #Perform sentiment analysis on the Tweets Data
  sentimentAnalyze <- function(tweets, worst, bad, good, best)
  {
   
    tweet_scores<-laply(tweets, function(message, worst, bad, good, best)
    {
      
      #Clean the tweet
      message <- gsub('[[:punct:]]', '', message)
      message <- gsub('[[:cntrl:]]', '', message)
      message <- gsub('\\d+', '', message)
      message <- tolower(message)
      
      
      split_tweets <- str_split(message, '\\s+')
      term <- unlist(split_tweets)
      bestmatch <- match(term, best)
      goodmatch <- match(term, good)
      worstmatch <- match(term, worst)
      badmatch<- match(term, bad)
      
      
      bestmatch <- sum(!is.na(bestmatch))
      goodmatch <- sum(!is.na(goodmatch))
      worstmatch <- sum(!is.na(worstmatch))
      badmatch <- sum(!is.na(badmatch))
      

      sentiment <- c(worstmatch, badmatch, goodmatch, bestmatch)
      return(sentiment)
    }, worst, bad, good, best)
    return(tweet_scores)
  }
  
  
  sentiment_columns <- sentimentAnalyze(tweets, worst, bad, good, best)
  

  
  
  col1 = sentiment_columns[,1]
  col2 = sentiment_columns[,2]
  col3 = sentiment_columns[,3]
  col4 = sentiment_columns[,4]
 
  #Amplify the sentiments by multiplying them with appropriate factors
  row1 = col1 * -10
  row2 = col2* -5
  row3 = col3 * 5
  row4 = col4 * 10 
  
  total_score = row1 + row2 + row3 + row4
  
  
  table1 <- data.frame("V-Negative" = col1, "Negative" = col2, "Positive" = col3, "V-Positive" = col4, "Aggregate Score" = total_score)
  
  table2 <- cbind(tweets,table1)
  
  View(table2)
  
  
  #Bar Chart
  xc<-c(sum(row1),sum(row2),sum(row3),sum(row4))
  names(xc) <- c("VeryNegative","Negative","Positive","VeryPositive")
  barplot(xc,col = c("red","darkorange","yellow","chartreuse2"))
  
  #PieChart
  slices <- c(abs(sum(row1)),abs(sum(row2)),sum(row3),sum(row4))
  pct <- round(slices/sum(slices)*100)
  lbls <- c("Very Negative","Negative","Positive","Very Positive")
  lbls <- paste(lbls,pct,"%")
  pie(slices,labels=lbls,col=c("red","darkorange","yellow","chartreuse2"),
        main="Pie Chart of Sentiment of Tweets ")

  
  #Wordcloud of the Tweets 
  
  tweet_text<- Corpus(VectorSource(tweets)) 
  tweet_text1 <- tm_map(tweet_text, PlainTextDocument)
  wc1 <- tm_map(tweet_text1,stripWhitespace)
  wc2 <- tm_map(wc1,removeNumbers)
  wc3 <- tm_map(wc2,removePunctuation)
  wc4 <- tm_map(wc3,removeWords,stopwords("english"))
  wc_final <- tm_map(wc4,content_transformer(stemDocument))
  
  wordcloud(words = wc_final,min.freq = 5,max.words = 500,random.order = 'false')
  
  