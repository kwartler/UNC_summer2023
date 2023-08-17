#Birnir scripts and HW.  Text analysis ICPSR
#Day 2
#Purpose: try to start working with papuan data
####################################################################################

#data
setwd("/Users/hannahbirnir/Dropbox/Papuan/data")
#load("/Users/hannahbirnir/Dropbox/Papuan/data/papuans_2022to2023.RData")

load("/Users/hannahbirnir/Dropbox/Papuan/data/paptwloc.Rda")

# Libs
library(tidyverse)
library(skmeans) #H:The main package
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(irlba) #H: Optimized version for principal component analysis
library(factoextra)
library(ggthemes)


tmp <- list.files(path = '~/Downloads', pattern = 'test.Rda', full.names = T)
load(tmp)


#subset to the using data including only location tagged posts
paptwloc=tweets_papuans_second%>%
  select(user_username, text, user_location)%>%
  filter(user_location!="")

paptwloc=paptwloc%>%
  arrange(user_location)

#The first 600 tweets still contain nonsensical locations.  Drop those.
paptwloc=paptwloc[-c(1:600), ]

#save data
save(paptwloc,file="paptwloc.Rda")

#1) Tag users who have Papua in their location

#I HAVE TO CONTINUE HUMAN TAGGING LOCATION (OR GO BACK TO SEE IF PULL HAS LONG LAT).  THIS IS NOT DONE
paptwloc=paptwloc%>%
mutate(paptwloc, locPapua = ifelse(grepl("Papua*",user_location),"Papua",
       ifelse(grepl("Indonesia*",user_location),"Indonesia",
      ifelse(grepl("Indonesia*",user_location),"jakarta*",  "NegaraAsing"))))

#IN THE MEANTIME I AM GOING TO SUBSET TO A SMALLER DATASET

set.seed(1234)
idx <- sample(1:nrow(paptwloc), 1000000)
test <- paptwloc[idx, ]

save(test,file="test.Rda")

#clean user location data
#TO DO: MODIFY CUSTOM FUNCTIONS TO SUIT DATA.  IN THE MEANTIME USE:

### TK
tmp <- list.files(path = '~/Downloads', pattern = 'test.Rda', full.names = T)
load(tmp)

testing <- T

if(testing==T){
  idx <- sample(1:nrow(test),10000)
  test <- test[idx, ]
}

#### Examine the languages
library(cld2)
tweetLanguages <- detect_language(test$text)
table(tweetLanguages)

#langugageKeeps <- tweetLanguages %in% c('en','id')
langugageKeeps <- tweetLanguages %in% c('en')
test <- test[langugageKeeps, ]
###


# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Stopwords
stops  <- c(stopwords('SMART'), 'amp')

# Custom Functions
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

# Let's loop here
allTxt <- vector()
for(i in 1:length(unique(test$locPapua))){
  print(unique(test$locPapua)[i])
  oneLoc <- subset(test, test$locPapua==unique(test$locPapua)[i])
  locTxt <- paste(oneLoc$text, collapse = ' ')

  # Apply tm functions
  txtCorp <- VCorpus(VectorSource(locTxt))
  txtCorp <- cleanCorpus(txtCorp, stops)

  allTxt[i] <- sapply(txtCorp, content)

}
allTxt <- VCorpus(VectorSource(allTxt))
allTxt <- TermDocumentMatrix(allTxt)
colnames(allTxt) <- unique(test$locPapua)

# Make comparison cloud
comparison.cloud(as.matrix(allTxt),
                 max.words=75,
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(allTxt),"Dark2"),
                 scale=c(3,0.1))





#Vizuals

###########################################
#Extra script
#"global substitution" remove everything in the class except ^,
#regular ASCII characters \x01-\x7F, replace with empty space ""

#PROBLEM this removes also script that is in japanese etc.
#Find a different solution
