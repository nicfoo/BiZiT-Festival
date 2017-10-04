enquiry <- 1
technicalSupport <-2
complaint <-3
request <-4
compliment <- 5
suggestion <- 6

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

newstopwords <- fread('http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop', header=FALSE)
#trim function to remove white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#wordLength function to get number of words
wordLength <- function (x) sapply(gregexpr("\\S+", x), length)
#====================================================================================================================

full_dataset <- read_csv("full_dataset.csv", 
                         col_types = cols(status_author_id = col_character()))
#Create training data
trgdata <- read_excel("C:/Users/attzhun1/Downloads/samplerawdata.csv.xlsx")
trgdata <- trgdata[,-1]

data <- iconv(trgdata$status_message, 'utf-8', 'ascii', sub="")
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
trgdata$status_message<-data$text


#train the data
rawdata <- full_dataset
rawdata$category <- NA
colnames(trgdata)[6] <- "category"
#rawdata$category<-trainingsample[match(rawdata$status_id, trainingsample$status_id),7]

#Create the test data
x <- trgdata$status_id
testdata <- rawdata[!rawdata$status_id%in%x,]

data <- iconv(testdata$status_message, 'utf-8', 'ascii', sub="")
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
testdata$status_message<-data$text

df <- data.frame(rbind(trgdata,testdata))

#Create DTM
doc_matrix <- create_matrix(df$status_message, language="english", removeNumbers=FALSE,
                            stemWords=FALSE, removeSparseTerms=.90)
container <- create_container(doc_matrix, df$category, trainSize=1:nrow(trgdata),
                              testSize=(nrow(trgdata)+1):nrow(doc_matrix), virgin=TRUE)
#RF
RF <- train_models(container, algorithms= c("RF"))
RF_CLASSIFY = classify_models(container, RF)
testRF <- testdata
testRF$category = RF_CLASSIFY$FORESTS_LABEL
rownames(testRF)<- NULL
testRF$status_message <- full_dataset[match(testRF$status_id,full_dataset$status_id),5]$status_message

#Convert the numbers to words
testRF$category <- ifelse(testRF$category == 1,"Enquiry",
                          ifelse(testRF$category ==2 ,"Tech Support",
                          ifelse(testRF$category==3,"Complaint",
                          ifelse(testRF$category == 4,"Request",
                          ifelse(testRF$category == 5, "Compliment",
                          ifelse(testRF$category == 6, "Suggestion/Others" ,0))))))

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

#------------------------------------------------Full data----------------------------------------------------------

trgdata <- read_excel("C:/Users/attzhun1/Downloads/samplerawdata.csv.xlsx")
trgdata <- trgdata[,-1]
colnames(trgdata)[6] <- "category"
trgdata$category <- ifelse(trgdata$category == 1,"Enquiry",
                         ifelse(trgdata$category ==2 ,"Tech Support",
                                ifelse(trgdata$category==3,"Complaint",
                                       ifelse(trgdata$category == 4,"Request",
                                              ifelse(trgdata$category == 5, "Compliment",
                                                     ifelse(trgdata$category == 6, "Suggestion/Others" ,0))))))

Final <- rbind(testRF, trgdata)
