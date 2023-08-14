#' Ted
#' Purpose: Explore some tweets
#' Aug 14, 2023

# Data
tmp <- list.files(path       = '~/Desktop/UNC_summer2023/extraTextData/random_sp500_transcripts',
                  pattern    = 'AAPL',
                  full.names = T)

transcript <- read.csv(tmp)

# Examine this data
names(transcript)
unique(transcript$speaker)

ceoTxt <- subset(transcript, transcript$speaker=='Timothy D. Cook')
ceoTxt$msg[1]

# Load libraries
library(tm)

# Custom Functions
# to lower
# Clean corpus
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create the Corpus
ceoCorpus <- VCorpus(VectorSource(ceoTxt$msg))

# Clean the corpus
stops     <- c(stopwords('english'), 'earnings', 'quarter', 'quarterly')
ceoCorpus <- cleanCorpus(ceoCorpus, customStopwords = stops)

# Save a copy of the cleaned data
cleanCook <- sapply(ceoCorpus, content)
writeLines(cleanCook, '~/Desktop/UNC_summer2023/personalFiles/cleanCook.txt')

# Create DTM
cookDTM <- DocumentTermMatrix(ceoCorpus)
cookDTM <- as.matrix(cookDTM)
cookDTM[1:3, 1:4]

# WFM
wordFreq <- colSums(cookDTM)
head(wordFreq)

wfm <- data.frame(word = colnames(cookDTM),
                  freq = wordFreq,
                  row.names = NULL)
head(wfm)

# Review the top 50 words
wfm <- wfm[order(wfm$freq, decreasing = T),]
head(wfm, 50)

# End
