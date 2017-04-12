# ---
# title: Data Science Project
# author: Rafael Leite
# ---

# Sets Work Directory
setwd("C:/Users/Rafael Zufi/Desktop/COSC 4931")

# Loads Data Set
dataset <- read.csv(file = "AirplaneCrashesSince1908.csv", header=TRUE, sep=",")

# Load libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("dplyr")
library("stringr")
library("tidyr")
library("factoextra")
library("ggplot2")

summary <- Corpus(VectorSource(dataset$Summary))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
summary <- tm_map(summary, toSpace, "/")
summary <- tm_map(summary, toSpace, "@")
summary <- tm_map(summary, toSpace, "\\|")

# Convert the text to lower case
summary <- tm_map(summary, content_transformer(tolower))

# Remove numbers
summary <- tm_map(summary, removeNumbers)

# Remove english common stopwords
summary <- tm_map(summary, removeWords, stopwords("english"))

# Remove selected stop words
summary <- tm_map(summary, removeWords, c("crashed", "aircraft", "flight", "plane"))

# Remove punctuations
summary <- tm_map(summary, removePunctuation)

# Eliminate extra white spaces
summary <- tm_map(summary, stripWhitespace)

dtm <- TermDocumentMatrix(summary)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Build wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))