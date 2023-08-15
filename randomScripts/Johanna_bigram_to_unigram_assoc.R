#' Ted
#' Aug 15, 2023
#' Quick bigram assocs w/unigrams
#'

# Declare the data path
filePath  <- 'https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/B_Basic_Visuals/data/chardonnay.csv'

# Libs
library(tm)
library(stringi)

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
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay')

# Data
text <- read.csv(filePath, header=TRUE)

######### Option1 - concatenate
text$text <- gsub('marvin gaye', 'marvingaye', text$text)

# Make a volatile corpus & unigram dtm
txtCorpus <- VCorpus(VectorSource(text$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
wineTDM  <- TermDocumentMatrix(txtCorpus)

# Inspect word associations
associations <- findAssocs(wineTDM, 'marvingaye', 0.1)
associations


######### WIP Option2 - count & append
# Tokens of interest
searchPattern <- 'marvin gaye'
searchCount   <- stri_count_fixed(text$text,
                                  pattern = searchPattern,
                                  case_insensitive = TRUE)

# Make a volatile corpus & unigram dtm
txtCorpus <- VCorpus(VectorSource(text$text))
txtCorpus <- cleanCorpus(txtCorpus, stops)
wineTDM  <- TermDocumentMatrix(txtCorpus)
wineTDMm <- as.matrix(wineTDM)
wineTDMm <- rbind(t(searchCount), wineTDMm)

# Examine
wineTDMm[1:4,1:6]

# Add in the rowname
rownames(wineTDMm)[1] <- searchPattern
wineTDMm[1:4,1:6]

# Convert back to
wineTDMm <- slam::as.simple_triplet_matrix(wineTDMm)
wineTDMm <- as.TermDocumentMatrix(wineTDMm, weighting = weightTf)


# Inspect word associations
associations <- findAssocs(wineTDMm, 'marvin gaye', 0.1)
associations

# End
