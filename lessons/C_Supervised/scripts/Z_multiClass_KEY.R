#' Multi-classification Models Example
#' Ted Kwartler
#' Aug 15, 2023

# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)

# Custom Functions
# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'),'bbc', 'business', 'entertainment', 'politics', 'sport', 'technology')

# Read in the data and organize it
allFolders <- list.dirs(path = '~/Desktop/UNC_summer2023/lessons/C_Supervised/data/bbc-fulltext/bbc')
allFolders

allArticles <- list()
for(i in 2:length(allFolders)){

  # Read in the data
  articles <- list.files(allFolders[i], full.names = T)
  tmpTxt   <- sapply(articles, readLines) # Each article is a vector of text by line
  tmpTxt   <- lapply(tmpTxt, paste, collapse = ' ') # Collapse the lines into a single article


  # Clean it up and extract the content
  tmpTxt   <- VCorpus(VectorSource(tmpTxt))
  tmpTxt   <- cleanCorpus(tmpTxt, stops)
  tmpTxt   <- sapply(tmpTxt, content)

  # Organize the output
  articleClass <- basename(allFolders[i])
  print(articleClass)
  response <- data.frame(txt = tmpTxt, y = articleClass)
  allArticles[[i]] <- response
}

# Change to a data frame for modeling
allTxt <- do.call(rbind, allArticles)

allTxt[1,]
allTxt[500,]

### SAMPLE : Shuffly to avoid auto-correlation & then patrition
set.seed(1234)
allTxt <- allTxt[sample(1:nrow(allTxt), nrow(allTxt)),]

idx      <- createDataPartition(allTxt$y,p=.7,list=F)
trainTxt <- allTxt[idx,]
testTxt  <- allTxt[-idx,]


### EXPLORE - do we want to do a word frequency of the training text?
# We can also use a loop to do it by class
# Or we could even make a comparison cloud etc.
table(trainTxt$y)

### MODIFY
# Initial iterator to make vocabulary
iterMaker <- itoken(trainTxt$txt,
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker,
                               stopwords=stopwords('SMART'),
                               ngram = c(1,3))
head(textVocab)
textVocab[10000:10500,]
nrow(textVocab)

#prune vocab to make DTM smaller
summary(textVocab)
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM
txtDTM <- create_dtm(iterMaker, vectorizer)
dim(txtDTM)


### MODEL - multiclassification
fit <- cv.glmnet(x      = txtDTM,
                 y      = as.factor(trainTxt$y),
                 family = "multinomial")

### ASSESS
# This builds 5 models and chooses the top probability for the final decision
# So coefficients vary across classes
coefficients(fit)$business
coefficients(fit)$entertainment
coefficients(fit)$sport
coefficients(fit)$tech

# Predictions across all classes then choose the highest probability from the group
fitProbs <- predict(fit, s = fit$lambda.min, newx = txtDTM, type = 'response')  # probabilities
tail(fitProbs)

# Now get the maximum column for each row; could be done in one line
maximumProbability <- apply(fitProbs, 1, which.max)
head(maximumProbability)

articleClass <- colnames(fitProbs)[maximumProbability]
head(articleClass)

# Let's organize our output
resultsDF <- data.frame(doc_id = 1:length(trainTxt$y),
                        actual = trainTxt$y,
                        class  = articleClass)
head(resultsDF)
table(resultsDF$class, resultsDF$actual)  ## CLEARLY OVERFIT; probably an article tag in there!

# End
