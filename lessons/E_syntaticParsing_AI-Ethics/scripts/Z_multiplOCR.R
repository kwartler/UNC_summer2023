# Multiple file OCR
# TK
# Aug 18, 2023

# lib
library(tesseract)

# Inputs
savePath <- '~/Desktop/UNC_summer2023/randomScripts/multipleOCR'

# Get all our jpg from the web into a vector
imgs <- c('https://github.com/kwartler/UNC_summer2023/blob/main/randomScripts/multipleOCR/imgs/Screenshot%202023-08-18%20at%2012.43.43%20PM.png?raw=true',
          'https://github.com/kwartler/UNC_summer2023/blob/main/randomScripts/multipleOCR/imgs/Screenshot%202023-08-18%20at%2012.43.55%20PM.png?raw=true',
          'https://github.com/kwartler/UNC_summer2023/blob/main/randomScripts/multipleOCR/imgs/Screenshot%202023-08-18%20at%2012.44.02%20PM.png?raw=true',
          'https://github.com/kwartler/UNC_summer2023/blob/main/randomScripts/multipleOCR/imgs/Screenshot%202023-08-18%20at%2012.44.18%20PM.png?raw=true',
          'https://github.com/kwartler/UNC_summer2023/blob/main/randomScripts/multipleOCR/imgs/Screenshot%202023-08-18%20at%2012.44.31%20PM.png?raw=true',
          'https://github.com/kwartler/UNC_summer2023/blob/main/randomScripts/multipleOCR/imgs/Screenshot%202023-08-18%20at%2012.44.41%20PM.png?raw=true')

# Load Language
eng  <- tesseract("eng")

# ocr() is vectorized!
text <- ocr(imgs, engine = eng)
text <- lapply(text, function(x){capture.output(cat(x))})
text <- lapply(text, paste, collapse = ' ')
text <- unlist(text)
write.csv(text,
          '~/Desktop/UNC_summer2023/randomScripts/multipleOCR/ocr_imgs.csv',
          row.names = F)

# Now more fidelity, to preserve output we need to use lapply
confidenceCutoff <- 70 # needs to be out of 100, not decimal
results <- lapply(imgs, ocr_data, engine = eng)
results <- lapply(results, as.data.frame)

highConfidenceCleaning <- function(x, confidenceCutoff){
  x <- subset(as.data.frame(x), x$confidence > confidenceCutoff)
  x <- paste(x$word, collapse = ' ')
  return(x)
}

cutOffResults <- lapply(results, highConfidenceCleaning, confidenceCutoff)

# Let's see if we can engineer the politifact results
polifactCheck <- function(x){

  chkIDX <- grep('FALSE', x$word, ignore.case = F)
  if(length(chkIDX)==0){
    chkIDX <- NA
  }
  x <- paste(x$word, collapse = ' ')
  # in this limited example, its either not found or is all caps
  # So choosing ignore.case = F; maybe change?
  chk <- grepl('FALSE', x, ignore.case = F)

  # Logical description
  chk <- ifelse(chk==T,'FALSE was identified', 'FALSE was NOT identified')
  response <- data.frame(falseFlag = chk, falseWordIndexRawTxt = chkIDX)
  return(response)
}
politiFactID <- lapply(results, polifactCheck)


# Organize into one CSV - cat(text) raw and also bring in cutoff threshold version
rawTxt <- lapply(results, function(x){paste(x$word, collapse = ' ')})
rawTxt <- unlist(rawTxt)
cutOffResults <- unlist(cutOffResults)
politiFactID <- do.call(rbind, politiFactID)

finalDF <- data.frame(rawTxt             = rawTxt,
                      cutOffResults      = cutOffResults,
                      foundFALSEinRawTxt = politiFactID$falseFlag,
                      falseWordIndex     = politiFactID$falseWordIndex)
finalDF[1,]

# Save files
write.csv(finalDF, '~/Desktop/UNC_summer2023/randomScripts/multipleOCR/ocr_data_with_cutoff.csv', row.names = F)

# End
