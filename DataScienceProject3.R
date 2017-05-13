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
library("ggplot2")
library("scales")
library("gridExtra")
library("caret")

dataset$Fatalities[is.na(dataset$Fatalities)] = 0

# Create Corpus
summary <- VCorpus(VectorSource(dataset$Summary))

# Convert the text to lower case
summary <- tm_map(summary, tolower)

# Keep text only
summary <- tm_map(summary, PlainTextDocument)

# Remove numbers
summary <- tm_map(summary, removeNumbers)

# Remove english common stopwords
summary <- tm_map(summary, removeWords, stopwords("english"))

# Remove punctuations
summary <- tm_map(summary, removePunctuation)

# Eliminate extra white spaces
summary <- tm_map(summary, stripWhitespace)

dtm = DocumentTermMatrix(summary)
dtm = removeSparseTerms(dtm, 0.95)

# Remove empty documents
nRows = apply(dtm , 1, sum)
dtm = dtm[nRows> 0, ]

dtm_tfxidf = weightTfIdf(dtm)

# K-means
m = as.matrix(dtm_tfxidf)
rownames(m) = 1:nrow(m)
preproc = preProcess(m)
m_norm = predict(preproc, m)
cl = kmeans(m_norm, centers = 5)

# Plot clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)

freq_terms_1 = findFreqTerms(dtm[cl$cluster==1,], 50)
freq_terms_2 = findFreqTerms(dtm[cl$cluster==2,], 50)
freq_terms_3 = findFreqTerms(dtm[cl$cluster==3,], 50)
freq_terms_4 = findFreqTerms(dtm[cl$cluster==4,], 50)
freq_terms_5 = findFreqTerms(dtm[cl$cluster==5,], 50)

print('Frequent words in cluster 1:')
for(i in freq_terms_1)
  cat(i, " ")
print('Frequent words in cluster 2:')
for(i in freq_terms_2)
  cat(i, " ")
print('Frequent words in cluster 3:')
for(i in freq_terms_3)
  cat(i, " ")
print('Frequent words in cluster 4:')
for(i in freq_terms_4)
  cat(i, " ")
print('Frequent words in cluster 5:')
for(i in freq_terms_5)
  cat(i, " ")

# Topic modeling
# Number of topics
k <- 5
 
# Run LDA using Gibbs sampling
ldaOut <- LDA(dtm, k, method = "Gibbs", control=list(nstart = 5, seed = list(2003,5,63,100001,765), best = TRUE, burnin = 4000, iter = 2000, thin = 500))

# Write out results
# Top 10 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms, file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

# Probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities, file=paste("LDAGibbs",k,"TopicProbabilities.csv"))