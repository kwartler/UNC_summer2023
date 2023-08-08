#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' Date: May 29, 2023
#'

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Options; google api returns UTF-8 text
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Youtube URL
# https://www.youtube.com/watch?v=K5Rly83zfuI&ab_channel=TheDailyShowwithTrevorNoah
youtubeCaption <- "https://www.youtube.com/api/timedtext?v=K5Rly83zfuI&ei=8LPSZM7wEYGcy_sP_cetCA&caps=asr&opi=112496729&xoaf=4&hl=en&ip=0.0.0.0&ipbits=0&expire=1691555424&sparams=ip%2Cipbits%2Cexpire%2Cv%2Cei%2Ccaps%2Copi%2Cxoaf&signature=3C84B6C7F7C9D467BF99B5A84640E345AAF2130C.9D62F6D27BD616CF1B9EDF31A5D705F701479083&key=yt8&lang=en-US&fmt=json3&xorb=2&xobt=3&xovt=3&cbrand=apple&cbr=Chrome&cbrver=114.0.0.0&c=WEB&cver=2.20230807.06.00&cplayer=UNIPLAYER&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP"

# Backup file
#youtubeCaption <- '~/Desktop/UNC_summer2023/lessons/D_Sentiment_APIs_GPT/data/f.json'

# Go get the data
dat <- fromJSON(youtubeCaption) # you can even pass in a URL to go to a webpage

# closed captioning data
dat$events$tStartMs
dat$events$dDurationMs
dat$events$segs[1:10]

# Get each first column called utf8
rawTxt <- lapply(dat$events$segs, "[", 'utf8')

# organize just the single column
rawTxt <- do.call(rbind, rawTxt)

# Drop line returns "\n"
rawTxt <- gsub('[\r\n]',' ',rawTxt[,1])

# Sometimes there are entries that are empty so they need to be dropped
head(rawTxt,10)
rawTxt <- rawTxt[nchar(rawTxt) != "0"]

# Sometimes, there is extra spacing from the gsub
rawTxt <- str_squish(rawTxt)

# If you want it as a single chunk
oneChunk <- paste(rawTxt, collapse = ' ')
oneChunk

# If you want to retain the meta data
textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = unlist(lapply(dat$events$segs, "[", 'utf8') ))

# Examine to make sure format is ok
head(textDF, 10)

# End
