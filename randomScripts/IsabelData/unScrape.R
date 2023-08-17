#' UN
#' TK
#' Aug 16, 2023
#'

#I Found 29 pages with information
# https://www.unfpa.org/search/content?search_api_fulltext=demographic%20dividend&page=29

# Construct all URLS
allUN <- paste0('https://www.unfpa.org/search/content?search_api_fulltext=demographic%20dividend&page=',1:29)

# Get all URLS that are press
allRelevantResults <- list()
for(i in 1:length(allUN)){
  print(i)
  pg <- read_html(allUN[i])

  # Results URLS
  unResults <- pg %>% html_nodes('.views-field-title')%>%
    html_nodes('a') %>% html_attr('href')
  allRelevantResults[[i]] <- unResults
}

# Complete & save the URLS
allRelevantVector <- unlist(allRelevantResults)
allRelevantVector <- paste0('https://www.unfpa.org', allRelevantVector)
writeLines(allRelevantVector,  '~/Desktop/UNC_summer2023/randomScripts/IsabelData/UN_searchResultsURLS.txt')

# Looks like there are press, news and event pages.  Will need to explore them.

# Example events page -- Success!
tmp <- read_html(allRelevantVector[5])
tmp <- tmp %>% html_nodes('.page-content') %>% html_text()
tmp

# Example press --partial success picks up extra text at the bottom
tmp <- read_html(allRelevantVector[142])
tmp <- tmp %>% html_nodes('.page-content') %>% html_text()
tmp

# Example news --partial success picks up extra text at the bottom
tmp <- read_html(allRelevantVector[121])
tmp <- tmp %>% html_nodes('.page-content') %>% html_text()
tmp

# Get rid of /n and make it into lines
rawTxt <- capture.output(cat(tmp))
rawTxt <- trimws(rawTxt)

# Initially we can cutoff anything after "Related Content"
cutoffLine <- grep('\\bRelated Content\\b', rawTxt)
trimws(paste(rawTxt[1:cutoffLine-1], collapse = ' '))
# End
