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

processed_data <- read_csv("C:/Users/attzhun1/Downloads/dashboard/processed_data.csv")

priority <- function(sentiment, category, label) {
  a<- ifelse(category == "Enquiry", 2,
         ifelse(category == "Tech Support", 4,
                ifelse(category=="Complaint", 3,
                       ifelse(category == "Request",3,
                              ifelse(category == "Compliment",1,0)))))
  
  b<-ifelse(sentiment == "Negative", 1.5,
            ifelse(sentiment == "Neutral", 1.2,
                   ifelse(sentiment=="Positive", 1, 0)))
  
  c<- ifelse(label == "Broadband", 2,
         ifelse(label == "Lifestyle Services", 1.3,
                ifelse(label=="Mobile", 2,
                       ifelse(label == "Others",1,
                              ifelse(label == "Telephone Services",1.5,0)))))
  
  return(a+(b*c))
}