#' Multi-classification Models Example
#' Ted Kwartler
#' Aug 15, 2023

# Load the text2vec,caret,tm, and glmnet libraries


# Custom Functions - tryTolower, cleanCorpus
# Options & Functions
Sys.setlocale('LC_ALL','C')



# Use the 'english' stop words and our five classes
# 'bbc' 'business' 'entertainment' 'politics' 'sport' 'technology'
stops <- c(stopwords('_____'),)

# Change the path to your /bbc path something like this:
# '~/Desktop/UNC_summer2023/lessons/C_Supervised/data/bbc-fulltext/bbc'
allFolders <- list.dirs(path = '______________________')
allFolders

# Fill in the correct parts of this loop
allArticles <- list()
for(i in 2:length(allFolders)){

  # Read in the data
  articles <- list.files(allFolders[i], full.names = T)
  tmpTxt   <- sapply(articles, readLines) # Each article is a vector of text by line
  tmpTxt   <- lapply(tmpTxt, paste, collapse = ' ') # Collapse the lines into a single article


  # Create a corpus using vector source functions
  tmpTxt   <- ______(______(tmpTxt))

  # Apply the cleaning function passing in the corpus from abov and the stop words
  tmpTxt   <- ________(____, _____)

  # This extracts the cleaned up article text by apply the content function to all docs
  tmpTxt   <- sapply(tmpTxt, content)

  # Organize the output
  articleClass <- basename(allFolders[i])
  print(articleClass)
  response <- data.frame(txt = tmpTxt, y = articleClass)
  allArticles[[i]] <- response
}

# Change to a data frame for modeling
allTxt <- do.call(rbind, allArticles)

# Examine the first article
allTxt[1,]

# Now examine the 500th
allTxt[____]

### SAMPLE : Shuffly to avoid auto-correlation & then patrition
set.seed(1234)
allTxt <- allTxt[sample(1:nrow(allTxt), nrow(allTxt)),]

idx      <- createDataPartition(allTxt$y,p=.7,list=F)
trainTxt <- allTxt[idx,]
testTxt  <- allTxt[-idx,]


### EXPLORE - do we want to do a word frequency of the training text?
# We can also use a loop to do it by class
# Or we could even make a comparison cloud etc.

# Tally the Y variable of trainTxt to understand the article distribution


### MODIFY
# Initial iterator to make vocabulary
# Tokenize the txt column of our training text
iterMaker <- itoken(____$____,
                    progressbar         = T)

# Here we have a new parameter:
# Pass in the itoken object
# Let's be more aggressive with our stop words and use the SMART lexicon
# let's add in the ngram parameter to get unigrams, bigrams, and trigrams
textVocab <- create_vocabulary(_____,
                               stopwords=stopwords('_____'),
                               _____ = c(1,3))

# Examine the result by looking at the top 6
head(textVocab)

# Now let's look at the 10000 to 10500
textVocab[______:_____,]

# How many ngrams (nrow) were identified?
____(_____)

# Prune vocab to make DTM smaller
# have a max proportion of 50%
# min should be .001
# min count is 10
summary(textVocab)
prunedtextVocab <- prune_vocabulary(____,
                                    term_count_min = __,
                                    doc_proportion_max = ___,
                                    doc_proportion_min = ____)

# Now what the total number of ngrams left?
____(_____)

# Using the pruned vocabulary to declare the DTM vectors in a new object called vectorizer
_______ <- vocab_vectorizer(______)

# Take the vocabulary lexicon and the pruned text function to make a DTM
txtDTM <- create_dtm(iterMaker, vectorizer)
dim(txtDTM)


### MODEL - multiclassification
# fit our model
# x inputs are the DTM
# Y is a factor of the training data $y variable
# Since this is multi-class we will leave it as "multinomial"
fit <- cv.glmnet(x      = ______,
                 y      = as._______(______$_),
                 family = "multinomial")

### ASSESS
# This builds 5 models and chooses the top probability for the final decision
# So coefficients vary across classes
coefficients(fit)$business
coefficients(fit)$entertainment
coefficients(fit)$sport
coefficients(fit)$tech

# Predictions across all classes then choose the highest probability from the group
# Call predict on the model object
# leave the penalty lambda as is
# the new x matrix is the DTM we created
# Instead of the type = 'class', now we need are going to change to 'response' to get probabilities
fitProbs <- _______(_____, s = fit$lambda.min, newx = _____, type = 'response')

# Examine the bottom 6 rows of the probabilities
____(_____)

# Now get the maximum column for each row; could be done in one line
# Since this is new for some programmers let's look at the documentation for apply
?apply
maximumProbability <- apply(fitProbs, 1, which.max)
head(maximumProbability)

articleClass <- colnames(fitProbs)[maximumProbability]
head(articleClass)

# Let's organize our output
# Pass in the training text $y as the actual column
# pass in the model's articleClass outcome for the class column
resultsDF <- data.frame(doc_id = 1:length(trainTxt$y),
                        actual = ____$_,
                        class  = ______)
head(resultsDF)

# Table the results of class by actual from the results data frame object
_____(_____$_____, ______$______)  ## CLEARLY OVERFIT; probably an article tag in there!

# End
