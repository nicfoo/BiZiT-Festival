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
full_dataset <- read_csv("C:/Users/attzhun1/Desktop/full_dataset.csv", 
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
  row.names(train_mat)[1] = ""
  row.names(train_mat)[2] = test_text
  p <- predict(classifier, train_mat[1:2,])
  as.character(p[2])
}



#corpus <- Corpus(VectorSource(full_dataset$status_message))
#clean <- tm_map(corpus, removeWords, newstopwords$V1)
#clean <- tm_map(clean, stripWhitespace)
#clean <- tm_map(clean, removePunctuation)
#clean <- tm_map(clean, toSpace, "$")
#clean <- tm_map(clean, content_transformer(tolower))
#clean <- tm_map(clean, stripWhitespace)
stat_msg <- data.frame(full_dataset$status_message)

#Remove non-alphabetical characters
stat_msg <- iconv(stat_msg$full_dataset.status_message, 'utf-8', 'ascii', sub="")
#Change to lower case
stat_msg <- tolower(stat_msg)
#Remove stopwords
stat_msg <- removeWords(stat_msg, newstopwords$V1)
#Remove punctuations
stat_msg <- str_replace_all(stat_msg,"[[:punct:]]","")

#Change to stemwords
stat_msg <- stemDocument(stat_msg, language="english")
stat_msg <- trim(stat_msg)
stat_msg <- data.frame(text=stat_msg)
#This is to remove empty rows
stat_msg$text[stat_msg$text==""] <- NA
stat_msg <- na.omit(stat_msg)
testdata <- create_matrix(stat_msg$text[1:100], language='english', removeNumbers=F, toLower=F, removePunctuation=F,stemWords=F, removeSparseTerms=0.998)
testmat <- as.matrix(testdata)
#corpus <- Corpus(VectorSource(stat_msg$text))
#statTDM <- TermDocumentMatrix(corpus)
#d <- dist(as.matrix(stat_msg$text))


#Taxonomy
cat <- data.frame(text = c("phone", "postpaid service", "prepaid service", "postpaid add-ons", "roaming", "fibre broadband plan", "broadband add-ons", "tv packs", "tv channels", "set-top box", "home line", "IDD calls", "singtel music", "spotify", "singtel dash", "remittance", "processes and policies", "installation", "incidents"), label = c("Mobile", "Mobile", "Mobile", "Mobile", "Mobile", "Broadband", "Broadband", "TV", "TV" , "TV", "Telephone Services", "Telephone Services", "Lifestyle Services","Lifestyle Services", "Lifestyle Services", "Others", "Others" , "Others", "Others") )
mat <- create_matrix(cat$text, language='english', removeNumbers=F, toLower=F, removePunctuation=F,stemWords=F, removeSparseTerms=0.998)
mat <- as.matrix(mat)
nb_classifier = naiveBayes(mat, as.factor(cat$label))
nb_results <- predict(nb_classifier,as.matrix(mat))
testcopy <- data.frame(text = stat_msg$text[1:100], label = test_results)

#test
cat(predictTest("broadband", mat, nb_classifier))

#test dataset
test_results <- predict(nb_classifier,testmat)
