#' Ted
#' Aug 15
#' Purpose: Load, clean, and build visuals for winemag-data
#'

# lib
library(ggplot2)
library(ggthemes)
library(tm)
library(wordcloud)
library(data.table)
library(slam)

# load data
tmp <- list.files(path       = '~/Desktop/UNC_summer2023/lessons/B_Basic_Visuals/data',
                  pattern    = 'winemag',
                  full.names = T)
wineData <- fread(tmp)
wineData <- as.data.frame(wineData)

# Examine this data
head(wineData)
wineVar <- as.data.frame(table(wineData$variety))
wineVar <- wineVar[order(wineVar$Freq, decreasing = T),]

wineCountry <- as.data.frame(table(wineData$country))
wineCountry <- wineCountry[order(wineCountry$Freq, decreasing = T),]

# clean and make into a corpus
countryWine <- wineData[wineData$country %in% wineCountry[2:6, 1], ]

# Custom functions
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

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "),
         use.names = FALSE)
}

# Define our stop words
stops <- c(stopwords('english'),'wine','alcohol','notes', 'ready',
           'drink','now')

# Cleaning time
txtCorpus <- VCorpus(VectorSource(countryWine$description))
txtCorpus <- cleanCorpus(txtCorpus, stops)
wineTDM   <- TermDocumentMatrix(txtCorpus)

# Use slam to get the term frequencies
wineWFM <- slam::row_sums(wineTDM)
wineWFM <- data.frame(bigramWords = names(wineWFM),
                      freq        = wineWFM,
                      row.names   = NULL)
wineWFM <- wineWFM[order(wineWFM$freq, decreasing = T), ]
topWFM  <- wineWFM[1:100,]

# viz 1 - overall word cloud
wordcloud(words = topWFM$bigramWords,
          freq  = topWFM$freq)
# viz 2- dendrogram
# Use sparse terms to reduce our TDM
reducedTDM   <- removeSparseTerms(wineTDM, sparse=0.97)
reducedTDMDF <- as.data.frame(as.matrix(reducedTDM))
hc <- hclust(dist(reducedTDMDF))
plot(hc,yaxt='n')

# Now perform an association of interesting terms
termsToAssociate <- c('blend', 'young')
associations <- findAssocs(reducedTDM, termsToAssociate, 0.1)
associations

# viz 3 - comparison cloud by some factor - top 5 varieties
# viz 4 - comparison cloud by some factor - countries 2:6
wineList <- list()
for(i in 1:length(unique(countryWine$country))){
  countryName   <- unique(countryWine$country)[i]

  print(paste('working on:', countryName))
  oneCountry    <- subset(countryWine, countryWine$country==countryName)
  countryCorpus <- VCorpus(VectorSource(oneCountry$description))
  countryCorpus <- cleanCorpus(countryCorpus, stops)
  countryTxt    <- sapply(countryCorpus, content)
  countryTxt    <- paste(countryTxt, collapse = ' ')

  wineList[[i]] <- countryTxt
}

# Organize into a vector
wineCountryVec <- unlist(wineList)

# Make another corpus, instead of single reviews, its by country
allCountries <- VCorpus((VectorSource(wineCountryVec)))

# Create a simple TDM
drinkTDM  <- TermDocumentMatrix(allCountries)
drinkTDM  <- as.matrix(drinkTDM)

# append the country names into the matrix
colnames(drinkTDM) <- unique(countryWine$country)

comparison.cloud(drinkTDM,
                 max.words=75,
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(drinkTDM),"Dark2"),
                 scale=c(3,0.1))

# End
