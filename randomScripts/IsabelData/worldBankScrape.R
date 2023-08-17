#' World Bank
#' TK
#' Aug 16, 2023
#'

# lib
library(rvest)
library(stringr)

# There are 8 search URL pages, and each URLS of individual articles.
# First let's get the 64 unique articles URLS
allURLS <- paste0('https://www.worldbank.org/en/search?q=demographic+dividend&currentTab=',1:8)

getIndividualURLS <- function(baseURL){
  # Get the section of the pg with 8 results
  basePG <- read_html(baseURL) %>%
    html_nodes('.all__listing_section') %>%html_text()

  # For some reason the ref tags weren't working so we use regex
  pattern <- "(https?://|www\\.)\\S+"

  # Extract URLs from the character vector
  worldBankURLS <- str_extract_all(basePG, pattern)

  # Unlist the URLs into a single vector
  worldBankURLS <- unlist(worldBankURLS)
  return(worldBankURLS)
}

# Loop apply the function
allArticleURLS <- lapply(allURLS, getIndividualURLS)
allArticleURLS <- unlist(allArticleURLS)

# Save a copy
writeLines(allArticleURLS,
           '~/Desktop/UNC_summer2023/randomScripts/IsabelData/WorldBank_allArticleURLS.txt')

# So these are all VERY different and some are just data files, for now, let's get the pdfs and talk through the rest since it will take more work to know what's exactly needed.
savePth <- '~/Desktop/UNC_summer2023/randomScripts/IsabelData/PDF_download/'
pdfGet  <- grep('.pdf$', allArticleURLS)
pdfURLs <- allArticleURLS[pdfGet]

# Loop download
for(i in 1:length(pdfURLs)){
  fileName <- tail(unlist(strsplit(allArticleURLS[i],'/')),1)
  if(endsWith(fileName, '.pdf')==F){
    fileName <- paste0(fileName, '.pdf')
  }
  fileName <- paste0(savePth, fileName)
  download.file(allArticleURLS[i], fileName)
  print(fileName)
}

#### UGGH SOME OF THESE ARE NOT WORKING?!



# End
