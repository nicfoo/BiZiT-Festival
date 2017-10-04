library(readr)
library(readxl)
library("plyr")
library("tm")
library("class")
library("RTextTools")
library("e1071")
library("stringr")
library("SnowballC")
library("data.table")
library("caret")
full_dataset <- read_csv("full_dataset.csv", 
                         col_types = cols(status_author_id = col_character()))

newstopwords <- fread('http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop', header=FALSE)
#trim function to remove white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#wordLength function to get number of words
wordLength <- function (x) sapply(gregexpr("\\S+", x), length)

predictTest <- function(test_text, mat, classifier){
  train_mat = mat[1:2,]
  train_mat[,1:ncol(train_mat)] = 0
  test_text = stemDocument(test_text)
  test_matrix = create_matrix(test_text, language="english",
                              removeStopwords=T, removeNumbers=T,toLower=T, removePunctuation=T)
  test_mat <- as.matrix(test_matrix)
  for(col in colnames(test_mat)){
    if(col %in% colnames(train_mat))
      
    {
      train_mat[2,col] = test_mat[1,col];
    }
  }
  #test_mat = as.matrix(t(test_mat))
  #row.names(train_mat)[1] = ""
  #row.names(train_mat)[2] = test_text
  test_text <- predict(classifier, train_mat[1:2,])
  test_text <- as.character(test_text[2])
}


#stat_msg <- data.frame(full_dataset$status_message)

process <- function(text) {
  #Remove non-alphabetical characters
  text <- iconv(text, 'utf-8', 'ascii', sub="")
  #Change to lower case
  text <- tolower(text)
  #Remove stopwords
  text <- removeWords(text, newstopwords$V1)
  #Remove punctuations
  text <- str_replace_all(text,"[[:punct:]]","")
  text <- str_replace_all(text,"[^[:graph:]]", " ") 
  
  #Change to stemwords
  text <- stemDocument(text, language="english")
  text <- trim(text)
  text <- data.frame(text=text)
  #This is to remove empty rows
  text$text[text$text==""] <- NA
  text <- na.omit(text)
}


#Taxonomy
#cat <- read_excel("C:/Users/attzhun1/Downloads/Taxonomy.xlsx")
#cat$text <- process(cat$text)
#mat <- create_matrix(cat$text, language='english', removeNumbers=F, toLower=F, removePunctuation=F,stemWords=F, removeSparseTerms=0.998)
#mat <- as.matrix(mat)

#SVM
#container <- create_container(mat, cat$label, trainSize = 1:62, virgin=FALSE)
#model <- train_model(container, 'SVM', kernel="linear", cost=1)

#Naive Bayes
#nb_classifier = naiveBayes(mat, as.factor(cat$label))
#nb_results <- predict(nb_classifier,as.matrix(mat))

#test process
#sampledata <- process(full_dataset$status_message)
#sampledata <- list(full_dataset$status_message)
#testdata <- create_matrix(sampledata$text, originalMatrix = mat, language='english', removeNumbers=F, toLower=F, removePunctuation=F,stemWords=F, removeSparseTerms=0.998)
#matsize <- length(sampledata)
#predictionContainer <- create_container(testdata, labels=rep(0,matsize), testSize=1:matsize, virgin=FALSE)

#testmat <- as.matrix(testdata)


#test
#cat(predictTest("broadband", mat, nb_classifier))

#test data
#test_results <- predict(model,testmat)
#results <- classify_model(predictionContainer, model)

#test results
#testcopy <- data.frame(text = sampledata, label = results$SVM_LABEL, prob = results$SVM_PROB)

#create training and test data
traindata <- full_dataset
data <- iconv(traindata$status_message, 'utf-8', 'ascii', sub="")
#Change to lower case
data <- tolower(data)
#Remove stopwords
data <- removeWords(data, newstopwords$V1)
#Remove punctuations
data <- str_replace_all(data,"[[:punct:]]","")
data <- str_replace_all(data,"[^[:graph:]]", " ") 

#Change to stemwords
data <- stemDocument(data, language="english")
data <- trim(data)
data <- data.frame(text=data)
data$text[data$text==""] <- NA
df <- full_dataset
df$status_message = data$text
df <- df[complete.cases(df),]
#df <- process(traindata$status_message)
df$Label <- NA


mobile <- "hi-card|talktim|nanosim|lg|incoming|outgoing|text|s2|s3|s4|note|phone|postpaid|htc|zen|prepaid|add-ons|roam|lumina|xperia|galaxy|iphon|data|call|huawei|appl|samsung|recept|2g|3g|4g|signal|sms|s8|s7|number|hp|sony|handphon|blackberri|android|mobil|sim card|simcard"
broadband <- "dc|jsp|isp|famili|fiber|dns|wireless|broadband|modem|fibr|broadband plan|router|wifi|network|broadband addon|ping|ip address"
tv<- "final|finals|liverpool|dortmund|man utd|video demand|bpl|tv|pack|broadcast|liga|channel|drama|miotv|mio|cabl|hd|settop box|box|epl|leagu|cup|uefa"
tele <- "home line|home|line|idd"
lifestyle <- "music|dash|spotifi"
others <-"onepass|elder|retail|bill|father|mother|complain|branch|concert|portal|websit|singnet|facebook|instagram|flickr|social media|remitt|custom|mysingtel|process|1688|general|inquiri|instal|app|email|incid|fb"

df[grep(others, df$status_message), "Label"] <- "Others"
df[grep(mobile, df$status_message), "Label"] <- "Mobile"
df[grep(broadband, df$status_message), "Label"] <- "Broadband"
df[grep(tv, df$status_message), "Label"] <- "TV"
df[grep(tele, df$status_message), "Label"] <- "Telephone Services"
df[grep(lifestyle, df$status_message), "Label"] <- "Lifestyle Services"


#Sample training
sample.data <- function (data, classi){
  trgdata <- data[data$Label == classi,]
  TOTAL=nrow(trgdata)
  set.seed(1)
  #sample 20% of Data as Training set
  TRAIN_VOL=floor(0.4*TOTAL)
  sample<-sample(1:TOTAL, TRAIN_VOL)
  trgdata=trgdata[sample,]
  return(trgdata)
}


#train data
trdata <- as.data.frame(df[complete.cases(df),])

#topicstring <- as.data.frame(df[complete.cases(df),])[,-1:-4]


#predictTopic <- function(entry,topicstring){
#  topic <- data.frame(status_message = entry, Label = NA)
 # df <- rbind(topicstring,topic)
  #doc_matrix <- create_matrix(df$status_message, language="english", removeNumbers=FALSE,
#                              stemWords=FALSE, removeSparseTerms=.90)
#  container <- create_container(doc_matrix, df$Label, trainSize=1:nrow(topicstring),
#                                testSize=(nrow(topicstring)+1):nrow(doc_matrix), virgin=TRUE)
#  RF <- train_models(container, algorithms= c("RF"))
 # result <- classify_models(container, RF)
#  return(result$FORESTS_LABEL)
#}
#topic <- data.frame(status_message = 'fhdshf', Label = NA)
#df <- rbind(topicstring,topic)

#predictTopic('mobile phone not working',topicstring,doc_matrix)

mob_sam <- sample.data(trdata, "Mobile")
bb_sam <- sample.data(trdata, "Broadband")
life_sam <- sample.data(trdata, "Lifestyle Services")
tele_sam <- sample.data(trdata, "Telephone Services")
tv_sam <- sample.data(trdata, "TV")
others_sam <- sample.data(trdata, "Others")
trdata <- data.frame(rbind(mob_sam,bb_sam,life_sam,tele_sam,tv_sam,others_sam))
#test data
testdata <- as.data.frame(df[!complete.cases(df),])
set.seed(1)
TOTAL <- nrow(testdata)
sample <- sample(1:TOTAL, floor(0.4*TOTAL))
testdata <- testdata[sample,]
#Join
df <- data.frame(rbind(trdata,testdata))



#Create DTM
doc_matrix <- create_matrix(df$status_message, language="english", removeNumbers=FALSE,
                            stemWords=FALSE, removeSparseTerms=.90)
container <- create_container(doc_matrix, df$Label, trainSize=1:nrow(trdata),
                              testSize=(nrow(trdata)+1):nrow(doc_matrix), virgin=TRUE)
#
# train a SVM Model
#
#SVM <- train_model(container,"SVM")
#
#SVM_CLASSIFY <- classify_model(container, SVM)
#
# Convert factors to characters
#
#SVM_CLASSIFY <- transform(SVM_CLASSIFY, SVM_LABEL = as.character(SVM_LABEL))
#
#head(SVM_CLASSIFY,20)
#testSVM <- testdata
#testSVM$Label = SVM_CLASSIFY$SVM_LABEL
#RF
RF <- train_models(container, algorithms= c("RF"))
RF_CLASSIFY = classify_models(container, RF)
testRF <- testdata
testRF$Label = RF_CLASSIFY$FORESTS_LABEL
rownames(testRF)<- NULL
testRF$status_message <- full_dataset[match(testRF$status_id,full_dataset$status_id),5]$status_message
colnames(testRF)[1] <- "Status ID"
colnames(testRF)[2] <- "Date Published"
colnames(testRF)[3] <- "Author"
colnames(testRF)[4] <- "Author ID"
colnames(testRF)[5] <- "Message"



#combined <- data.frame(text=(testSVM$text), SVMLabel=(testSVM$Label), RFLabel=(testRF$Label))
replymsg <- function(id) {
  link <- paste("https://www.facebook.com/singtel/posts/",substring(id, 13), sep="")
}

#-------------------------------------------------------------------------------
singtelfaq <- read_excel("singtelfaq.xlsx")
data <- na.omit(singtelfaq)
data$Question <- process(data$Question)
data <- data.frame(data$Question, Answer=data$Answer)
data_unique <- unique(data)

matrix <- create_matrix(data_unique$text, language='english', removeNumbers=F, toLower=F, removePunctuation=F, 
                        stemWords=F, removeSparseTerms=0.998)
mat <- as.matrix(matrix)

#SVM
container <- create_container(matrix, data_unique$Answer, trainSize=1:45, virgin = FALSE)
model <- train_model(container, "SVM", kernel="linear", cost=1)

#cat(predictTest("Where to recontract my plan?", mat, model))

#====================================================================================
#Sentiment
posText <- read.delim("positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))

negText <- read.delim("negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
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

predictSentiment = function(entry){
  score <- score.sentiment(entry, posText, negText,.progress = 'none')
  ifelse(score$score >0,"Positive",ifelse(score$score < 0,"Negative",ifelse(score$score==0,"Neutral")))
}



#testRF$Sentiment <- NA


scores = score.sentiment(testRF$Message, posText,negText , .progress='text')

scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score <0)
scores$neutral <- as.numeric(scores$score==0)
sum(scores$positive)
sum(scores$neutral)
sum(scores$negative)
testRF$Sentiment <- ifelse(scores$score >0,"Positive",ifelse(scores$score < 0,"Negative",ifelse(scores$score==0,"Neutral",0)))

#Hierarchical Clustering -> Dendrogram
#d <- dist(rawdata,method = "euclidean")
#fit <- hclust(d)

#=====================================================================
priority <- function(sentiment, category, label, timedata) {
  a<- ifelse(category == "Enquiry", 2,
             ifelse(category == "Tech Support", 4,
                    ifelse(category=="Complaint", 3,
                           ifelse(category == "Request",3,
                                  ifelse(category == "Compliment",1,1)))))
  
  b<-ifelse(sentiment == "Negative", 1.5,
            ifelse(sentiment == "Neutral", 1.2,
                   ifelse(sentiment=="Positive", 1, 0)))
  
  c<- ifelse(label == "Broadband", 2,
             ifelse(label == "Lifestyle Services", 1.3,
                    ifelse(label=="Mobile", 2,
                           ifelse(label == "Others",1,
                                  ifelse(label == "Telephone Services",1.5,0)))))
  
  datetime <- as.Date(substring(timedata, 1, 10))
  datetime <- as.numeric(Sys.Date() - datetime)
  
  return(a+(b*c) + (datetime*0.1))
}
#With priority inside
finaldata <- read.csv("processed_data.csv")
finaldata <- finaldata[,-1]
finaldata <- finaldata[,-9]
finaldata$Priority <- priority(finaldata$Sentiment, finaldata$Category, finaldata$Label, finaldata$status_published)
rownames(finaldata) <- NULL
colnames(finaldata)[1] <- "Status ID"
colnames(finaldata)[2] <- "Date Published"
colnames(finaldata)[3] <- "Author"
colnames(finaldata)[4] <- "Author ID"
colnames(finaldata)[5] <- "Message"
finaldata$Message <- full_dataset[match(finaldata$`Status ID`,full_dataset$status_id),5]$status_message


set.seed(12)
dsample <- sample(1:nrow(finaldata), 500)
dashsample <- finaldata[dsample,] 
rownames(dashsample) <- NULL
