#' Purpose: Other sentiment libraries
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 5, 2023


# Libraries
library(rvest)

# Two data sets
transcriptNVDIA <- 'https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/D_Sentiment_APIs_GPT/data/transcriptNVDIA.txt'
pgNvidia <- readLines(transcriptNVDIA)
#writeLines(pgNvidia, 'transcriptNVDIA.txt')

transcriptFoxConn <- 'https://raw.githubusercontent.com/kwartler/UNC_summer2023/main/lessons/D_Sentiment_APIs_GPT/data/transcriptFox.txt'
pgFox <- readLines(transcriptFoxConn)

# Prepare - tolower, stopwords

# Search for terms

# Lexicons

# Measure polarity/sentiment

# Visuals

# End
