# Ted
# Read and organize an RTF
# Aug 14

# libraries - had to get an old package
#install.packages('antiword')
#install.packages("~/Downloads/textreadr_1.2.0.tar.gz", repos = NULL, type = "source")
library(textreadr)

# data
tmp <- list.files(path       = '~/Desktop',
                  pattern    = 'Canadian',
                  full.names = T)

# Read in

info <- textreadr::read_docx(tmp[2])
head(info,10)

# Article Break
splitPattern <- '____________________________________________________________'
idx <- grep(splitPattern, info)

# Create an empty list to store the text sections
sections <- list()

# Loop through the index positions to get individual sections
for (i in 1:(length(idx) + 1)) {
  start <- ifelse(i == 1, 1, idx[i-1] + 1)
  end <- ifelse(i > length(idx), length(info), idx[i])
  section <- paste(info[start:end], collapse = "\n")
  section <- capture.output(cat(section))
  sections[[i]] <- section
}
sections[[1]]
sections[[2]]
sections[[3]]

# Now for each section get the full text and the date
tmpDF <- list()
for(i in 1:length(sections)){
  print(paste('working on ',i,'of',length(sections)))
  oneSection <- sections[[i]]

  dateCheck <- grep('Last updated:', oneSection)
  if(length(dateCheck)>0){
    lastUpdated <- oneSection[dateCheck]
    lastUpdated <- gsub('Last updated: ','',lastUpdated)
  } else {
    lastUpdated <- NULL
  }

  textCheck <- grep('Full text:', oneSection)
  if(length(textCheck)>0){
    fullTxt <- oneSection[textCheck:length(oneSection)]
    en <- grep("[^:]+:\\s", fullTxt)
    justTxt <- paste(fullTxt[1:en[2]], collapse = ' ')

    # Audio versions cause an issue so quick fix where it ends on Subject:
    if(nchar(justTxt)<256){
      newEnd <- grep('Subject:', fullTxt)
      if(length(newEnd)==0){newEnd <- en} # Avoids an error but pretty cludgy!
      justTxt <- paste(fullTxt[1:newEnd-1], collapse = ' ')
    }

  } else {
    justTxt <- NULL
  }

  resp <- data.frame(lastUpdated = lastUpdated, article = justTxt)

  tmpDF[[i]] <- resp
}

finalDF <- do.call(rbind, tmpDF)
write.csv(finalDF, '~/Desktop/UNC_summer2023/randomScripts/BrooksData/articles2.csv',
          row.names = F)

# Looks like 3 were still mis parsed
summary(nchar(finalDF$article))
plot(density(nchar(finalDF$article)))

# Rows 379, 1351, 1379
subset(finalDF, nchar(finalDF$article)<1)
# End


