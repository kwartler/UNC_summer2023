# Get CFPB Complaints
# Build frequent terms timelines
# Ted
# Aug 14, 2023

# Libs
library(data.table)
library(lubridate)
library(tm)
library(dplyr)
library(ggplot2)

# Custom Functions
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

# Data downloaded from
# https://www.consumerfinance.gov/data-research/consumer-complaints/search
# Saved a toy data set for people to practice
complaints <- fread('~/Downloads/complaints-2023-08-14_19_33.csv')

# Colnames need help
names(complaints) <- make.names(names(complaints))

# Make a date class
complaints$Date.received <- mdy(complaints$Date.received )

# Append a grouping temporal var
complaints$yr      <- year(complaints$Date.received)
complaints$mth     <- month(complaints$Date.received, label = TRUE)
complaints$wk      <- week(complaints$Date.received)
complaints$weekday <- wday(complaints$Date.received, label=TRUE)

# Examine
head(complaints)

# Let's have a function to make it more performant
#' @param df the complaint data frame
#' @param period string must be a grouping variable from the column names mth wk weekday
#' @param stops vector of terms to remove
#' @param rmLongTail Boolean to remove the long tail frequency; default is T
#' @param testing Boolean if TRUE will randomly sample 10k complaints instead of all
freqWordTime <- function(df,
                         period     = 'mth',
                         stops      = stopwords('SMART'),
                         rmLongTail = T,
                         testing    = T){

  if(testing==T){
    set.seed(1234)
    idx <- sample(1:nrow(df), 10000)
    df  <- df[idx,]
  }

  allYrWFM <- list() # could also use dplyr
  for(i in 1:length(unique(df$yr))){
    print(paste('working on ',unique(df$yr)[i]))
    oneYr <- subset(df, df$yr==unique(df$yr)[i])
    oneYr <- oneYr[order(oneYr$Date.received), ]
    oneYr <- as.data.frame(oneYr)

    colIdx <- grep(period,names(df))
    periodValues <- list()
    for(j in 1:unique(oneYr[,colIdx])){
      cat(paste(unique(oneYr[,colIdx])[j]),'\n')
      oneYrOnePeriod <- subset(oneYr, oneYr[,colIdx]==unique(oneYr[,colIdx])[j])
      # Get the WFM
      txtCorpus <- VCorpus(VectorSource(oneYrOnePeriod$Consumer.complaint.narrative))
      txtCorpus <- cleanCorpus(txtCorpus, stops)
      txtDtm    <- DocumentTermMatrix(txtCorpus)
      txtDtmM   <- slam::col_sums(txtDtm)
      topTerms  <- data.frame(terms      = names(txtDtmM),
                              freq       = txtDtmM,
                              yr         = unique(df$yr)[i],
                              period     = unique(oneYr[,colIdx])[j],
                              row.names = NULL)

      if(rmLongTail==T){
        drops <- quantile(topTerms$freq, probs = seq(0, 1, 1/20))
        topTerms <- subset(topTerms, topTerms$freq>drops[20])
      }
      periodValues[[j]] <- topTerms
    }
    periodValues  <- do.call(rbind, periodValues)
    allYrWFM[[i]] <- periodValues
  }
  allYrWFM <- do.call(rbind, allYrWFM)
  return(allYrWFM)
}

stopWordVec <- c(stopwords('SMART'), 'CFPB')

# Remove the redacted info; WIP
complaints$Consumer.complaint.narrative <- gsub("(?i)X{3,}",
                                                "",
                                                complaints$Consumer.complaint.narrative,
                                                perl = TRUE)

complaints$Consumer.complaint.narrative <- gsub("[xX]{3,}", "", complaints$Consumer.complaint.narrative)

# Warnings appear with testing==T, all periods dont exist across all years
allWFM <- freqWordTime(df         = complaints,
                       period     = 'mth',
                       stops      = stopWordVec,
                       rmLongTail = T,
                       testing    = T)
head(allWFM)


# We dont want too many lines(terms) so again we can subset;
# multiple ways to do this - get top 10 terms across the whole data set
topN <- 12
topTerms <- allWFM %>%
  group_by(terms) %>%
  summarise(total_frequency = sum(freq)) %>%
  top_n(topN, total_frequency)

# Now subset the dataset to the identified terms preserving periodicity
plotDF <- allWFM[allWFM$terms %in% topTerms$terms,]

# Get the terms in common by year; 1st make the month complete
plotDF <- plotDF %>%
  mutate(date = ymd(paste(yr, period, "01", sep = "-")))

# Plot - WHEN TESTING IS TRUE THIS IS INACCURATE, w/missing information on x-asis
ggplot(plotDF, aes(x = date, y = freq, color = terms)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Frequency", color = "Term",
       title = "Term Frequency Timeline") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "none") +
  facet_wrap(~terms)
ggsave('testing_example_term_timelines.jpg')

