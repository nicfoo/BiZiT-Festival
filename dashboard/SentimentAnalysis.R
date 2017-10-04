#Open File
rawdata <- read_csv("C:/Users/ZhunHung/Desktop/full_dataset.csv", 
                       col_types = cols(status_author_id = col_character()))
statusMessage <- rawdata$status_message


library(readr)
library("plyr")
library("tm")
library("class")
library("RTextTools")
library("e1071")
library("stringr")
library("SnowballC")
library("data.table")
library("caret")

#Setting up the +ve and -ve texts
posText <- read.delim("C:/Users/ZhunHung/Downloads/BIZIT Festival/dashboard/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))

negText <- read.delim("C:/Users/ZhunHung/Downloads/BIZIT Festival/dashboard/negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
negText = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical','fuck','f***k')

#converting messages to text format
#message_txt <- read.delim("statusmessage.txt", header=TRUE, stringsAsFactors=FALSE)
#message_txt <- message_txt$V1
#messageVector <- unlist(lapply(message_txt, function(x) { str_split(x, "\n") }))
#numOfMessage <- length(message_txt)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of positive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  # create a simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}


scores = score.sentiment(statusMessage, posText,negText , .progress='text')

scores$positive <- as.numeric(scores$score > 0)
scores$negative <- as.numeric(scores$score < 0)
scores$neutral <- as.numeric(scores$score == 0)

sum(scores$positive)
sum(scores$neutral)
sum(scores$negative)
scores$polarity <- ifelse(scores$score >0,"positive",ifelse(scores$score < 0,"negative",ifelse(scores$score==0,"Neutral",0)))

#Sentiment Score plots
qplot(factor(scores$score), data=scores, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores")

#Sentiment +ve netural or -ve
qplot(factor(scores$polarity), data=scores, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments")


sd(scores$score)
min(scores$score)
max(scores$score)
mean(scores$score)


