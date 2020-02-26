rm(list=ls())
#load packages
library(readr)
library(DBI)
library(RMySQL)
library(tm)
require("NLP")
#install.packages("NMF")
require("openNLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library(NMF)
library(plyr) #weight sentiment
#install.packages("rJava")
#install.packages("Rwordseg")
library(rJava)
library(Rwordseg)
#access to text data
# driver <- dbDriver("MySQL")
# myhost <- "localhost"
# mydb   <- "studb"
# myacct <- "cis434"
# mypwd  <- "LLhtFPbdwiJans8F@S207" 
# 
# conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
# 
# #LOAD TEXT DATA
# # rO(x6c15'Can
# temp <- dbGetQuery(conn, "SELECT * FROM proj4final WHERE tag=\"rO(x6c15'Can\"")
# dbDisconnect(conn)


####load raw data
temp <- read.csv("~/Desktop/rstudio-export/temp.csv")
####load pos/neg dictionary "posneg"
# positive words ----label +1
pos <- read.csv("~/Desktop/rstudio-export/positive-words-dic.txt", header = T, sep = ",", stringsAsFactors = F)
weight <- rep(1, length(pos[,1]))
pos <- cbind(pos, weight)
names(pos) <- c("term", "weight")
#pos <- tolower(pos)
# negative words ----label -1
neg <- read.csv("~/Desktop/rstudio-export/negative-words-dic.txt", header = T, sep = ",", stringsAsFactors = F)
weight <- rep(-1, length(neg[,1]))
neg <- cbind(neg, weight)
names(neg) <- c("term", "weight")



# assign special weights to the negative words that I found from LDA
#They are: bad;delay;delayed;lost; last;trying; sucks; try; sucks; late; 
###########rudest;stuck;wait;waiting;cancel;cancelled;shitty; stupid; ashamed
neg$weight[which(neg$term =="bad")]=-100
neg$weight[which(neg$term =="delay")]=-100
neg$weight[which(neg$term =="delayed")]=-100
neg$weight[which(neg$term =="lost")]=-100
neg$weight[which(neg$term =="last")]=-100
neg$weight[which(neg$term =="trying")]=-100
neg$weight[which(neg$term =="sucks")]=-100
neg$weight[which(neg$term =="try")]=-100
neg$weight[which(neg$term =="late")]=-100
neg$weight[which(neg$term =="rudest")]=-100
neg$weight[which(neg$term =="stuck")]=-100
neg$weight[which(neg$term =="wait")]=-100
neg$weight[which(neg$term =="Waiting")]=-100
neg$weight[which(neg$term =="waiting")]=-100
neg$weight[which(neg$term =="cancel")]=-100
neg$weight[which(neg$term =="cancelled")]=-100
neg$weight[which(neg$term =="Cancel")]=-100
neg$weight[which(neg$term =="cancellation")]=-100
neg$weight[which(neg$term =="Thieves")]=-100
neg$weight[which(neg$term =="ashamed")]=-100
neg$weight[which(neg$term =="shitty")]=-100
neg$weight[which(neg$term =="stupid")]=-100


# combine pos and neg words
posneg <- rbind(pos, neg) 
posneg <- posneg[!duplicated(posneg$term), ]

#`duplicated` is similar to `unique`，but it can return to its own number


########divide words into vectors in each tweet
#make them to each string
sentence <- as.vector(temp$tweet)
#clean text
sentence= gsub("[[:punct:]]", "", sentence )
sentence = gsub("[[:digit:]]", "", sentence )
sentence <- tolower(sentence)


#install.packages("tokenizers")
library("tokenizers")
x<-tokenize_words(sentence, strip_punct = FALSE)

term <- unlist(x)  
temp0 <- lapply(x, length)                       
temp0 <- unlist(temp0)    
id <- rep(temp$id, temp0)  

#create a data frame that contains terms and their ids
testterm <- as.data.frame(cbind(id, term), stringsAsFactors = F) 


# create a set of vocabulary and clean data with elimiating stop words

stopwords<-data.frame(stopwords(kind='en'))
names(stopwords) <- c('term')
stopwords <- data.frame(setdiff(stopwords$term,posneg$term))
names(stopwords) <- c('term')
testterm <- testterm[!testterm$term %in% stopwords,]

#weight each review and weight with pos/neg dictionary
library(plyr)

testterm <- merge(testterm, posneg)
testterm <- testterm[!is.na(testterm$weight), ]
#head(testterm)
#computing sentiment index
dictresult <- aggregate(weight ~ id, data = testterm, sum)
dictlabel <- rep(-1, length(dictresult[, 1]))

#convert dictlabel which contains sign of sentiment to the data frame 
dictlabel[dictresult$weight > 0] <- 1 
dictresult <- as.data.frame(cbind(dictresult, dictlabel), stringsAsFactors = F)
###getting data frame only with non negative review
text <- join(dictresult, temp, by="id")
nonNeg <- subset(text, weight>0)
non_Negative <- nonNeg

#Finish non-negative data frame
non_Negative$weight <- NULL
non_Negative$dictlabel<-NULL
non_Negative$tag<-NULL
non_Negative$airline<-NULL
non_Negative$tid_not_to_be_used<-NULL
non_Negative$Evaluation <-1
non_Negative <- non_Negative[, c("id", "Evaluation", "tweet")]

#export non_Negative

write.csv(non_Negative, "non_Negative.csv")
dim(non_Negative) #160 non negative reviews

