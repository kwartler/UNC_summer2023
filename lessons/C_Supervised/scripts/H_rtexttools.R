#' Title: Multiple Supervised Methods
#' Purpose: Explore the rtexttools package
#' Author: Ted Kwartler
#' email: edward.kwartler@hult.edu
#' License: GPL>=3
#' Date: Aug 15, 2023
#'


# Libs
library(tm)
library(RTextTools)
library(yardstick)

# Custom helper functions
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


# Options & Functions
Sys.setlocale('LC_ALL','C')

# Create custom stop words
stops <- c(stopwords('SMART'), 'diabetes', 'patient')

# Get data
filePath <- 'https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/C_Supervised/data/diabetes_subset_8500.csv'
diabetes <- read.csv(filePath)
txt <- paste(diabetes$diag_1_desc,diabetes$diag_2_desc,diabetes$diag_3_desc)
txt <- stringi::stri_encode(txt, "", "UTF-8")

# Subset to avoid overfitting
set.seed(1234)
idx <- sample(1:length(txt), floor(length(txt)*.70))
train <- txt[idx]
test  <- txt[-idx]

# Clean, extract text and get into correct object
cleanTrain <- cleanCorpus(VCorpus(VectorSource(train)), stops)
cleanTrain <- data.frame(text = unlist(sapply(cleanTrain, `[`, "content")),
                       stringsAsFactors=F)

# This is not our original DTM function, its from RTextTools!
trainDTMm <- create_matrix(cleanTrain, language="english")

# Create the container
# trainSize; if you want to split within the single matrix but best practice is to bring it in separate to mimic really new data processing
container <- create_container(matrix    = trainDTMm,
                              labels    = diabetes$readmitted[idx],
                              trainSize = 1:length(idx),
                              virgin=FALSE)

# Build Models, can take ages for complex algos
#models <- train_models(container, algorithms=c("GLMNET","SVM")) #"SVM","SLDA","BOOSTING","BAGGING", "RF","GLMNET","TREE","NNET"
#saveRDS(models, 'rtexttools_models.rds')
models <- readRDS('rtexttools_models.rds')


# Score the original training data
results <- classify_models(container, models)
head(results)

# Append Actuals
results$actual <- diabetes$readmitted[idx]

# Confusion Matrix
table(results$GLMNET_LABEL, results$actual)
table(results$SVM_LABEL, results$actual)

# Accuracy GLMNET_LABEL
autoplot(conf_mat(table(results$GLMNET_LABEL, results$actual)))
accuracy(table(results$GLMNET_LABEL, results$actual))

# Accuracy SVM_LABEL
autoplot(conf_mat(table(results$SVM_LABEL, results$actual)))
accuracy(table(results$SVM_LABEL, results$actual))

# Now let's apply the models to "new" documents
# Clean, extract text and get into correct object
cleanTest <- cleanCorpus(VCorpus(VectorSource(test)), stops)
cleanTest <- data.frame(text = unlist(sapply(cleanTest, `[`, "content")),
                       stringsAsFactors=F)

# You have to combine the matrices to the original to get the tokens joined
allDTM <- rbind(cleanTrain, cleanTest)
allDTMm <- create_matrix(allDTM, language="english")
containerTest <- create_container(matrix    = allDTMm,
                                  labels    = diabetes$readmitted,
                                  trainSize = 1:length(idx),
                                  testSize  = (length(idx)+1):8500,
                                  virgin=T)

#testFit <- train_models(containerTest, algorithms=c("GLMNET", "SVM"))
#saveRDS(testFit, 'rtexttools_testFit.rds')
testFit <-readRDS('rtexttools_testFit.rds')
resultsTest <- classify_models(containerTest, testFit)

# Append Actuals
resultsTest$actual <- diabetes$readmitted[-idx]

# Confusion Matrix
summary(resultsTest$GLMNET_PROB)
summary(resultsTest$SVM_PROB)
table(resultsTest$SVM_LABEL, resultsTest$actual)
table(resultsTest$GLMNET_LABEL, resultsTest$actual)

# End
